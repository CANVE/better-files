package better.files

import java.io.{File => JFile, FileSystem => JFileSystem, _} //TODO: Scala 2.10 does not like java.io._
import java.net.URI
import java.nio.channels.{AsynchronousFileChannel, FileChannel}
import java.nio.file._, attribute._
import java.security.MessageDigest
import java.time.Instant
import java.util.zip.{Deflater, ZipFile}
import javax.xml.bind.DatatypeConverter

import scala.collection.JavaConversions._
import scala.io.{BufferedSource, Codec, Source}
import scala.util.Properties

/**
 * Scala wrapper around java.nio.files.Path
 */
class File(private[this] val _path: Path) {
  val path = _path.normalize.toAbsolutePath

  def pathAsString: String = path.toString

  def toJava: JFile = path.toFile

  def name: String = path.getFileName.toString

  def root: File = path.getRoot

  def nameWithoutExtension: String = if (hasExtension) name.substring(0, name lastIndexOf ".") else name

  /**
   * @return extension (including the dot) of this file if it is a regular file and has an extension, else None
   */
  def extension: Option[String] = when(hasExtension)(name.substring(name lastIndexOf ".").toLowerCase)

  def hasExtension: Boolean = (isRegularFile || notExists) && (name contains ".")

  /**
   * Changes the file-extension by renaming this file; if file does not have an extension, it adds the extension
   * Example usage file"foo.java".changeExtensionTo(".scala")
   */
  def changeExtensionTo(extension: String): File = if (isRegularFile) renameTo(s"$nameWithoutExtension$extension") else this

  def contentType: Option[String] = Option(Files.probeContentType(path))

  /**
   * Return parent of this file
   * NOTE: This API returns null if this file is the root;
   *       please wrap it in an Option if you expect to handle such behaviour
   *
   * @return
   */
  def parent: File = Option(path.getParent).map(pathToFile).orNull

  def /(child: String): File = path.resolve(child)

  def createChild(child: String, asDirectory: Boolean = false)(implicit attributes: File.Attributes = File.Attributes.default): File = (this / child).createIfNotExists(asDirectory)(attributes)

  def createIfNotExists(asDirectory: Boolean = false)(implicit attributes: File.Attributes = File.Attributes.default): File = if (exists) {
    this
  } else if (asDirectory) {
    createDirectories()(attributes)
  } else {
    parent.createDirectories()
    Files.createFile(path, attributes: _*)
  }

  def exists(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): Boolean = Files.exists(path, linkOptions: _*)

  def notExists(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): Boolean = Files.notExists(path, linkOptions: _*)

  def sibling(name: String): File = path resolveSibling name

  def isSiblingOf(sibling: File): Boolean = sibling isChildOf parent

  def siblings: Files = parent.list.filterNot(_ == this)

  def isChildOf(parent: File): Boolean = parent isParentOf this

  /**
   * Check if this directory contains this file
   * @param file
   * @return true if this is a directory and it contains this file
   */
  def contains(file: File): Boolean = isDirectory && (file.path startsWith path)
  def isParentOf(child: File): Boolean = contains(child)

  def bytes: Iterator[Byte] = newInputStream.buffered.bytes //TODO: ManagedResource here?

  def loadBytes: Array[Byte] = Files.readAllBytes(path)
  def byteArray: Array[Byte] = loadBytes

  def createDirectory()(implicit attributes: File.Attributes = File.Attributes.default): File = Files.createDirectory(path, attributes: _*)

  def createDirectories()(implicit attributes: File.Attributes = File.Attributes.default): File = Files.createDirectories(path, attributes: _*)

  def chars(implicit codec: Codec): Iterator[Char] = newBufferedReader(codec).chars //TODO: ManagedResource here?

  /**
    * Load all lines from this file
    * Note: Large files may cause an OutOfMemory in which case, use the streaming version @see lineIterator
    *
    * @param codec
    * @return all lines in this file
    */
  def lines(implicit codec: Codec): Traversable[String] = Files.readAllLines(path, codec)

  //TODO: def lineIterator(implicit codec: Codec) = Files.lines(path, codec).autoClosed.map(stream => asScalaIterator(stream.iterator()))

  def contentAsString(implicit codec: Codec): String = new String(byteArray, codec)
  def `!`(implicit codec: Codec): String = contentAsString(codec)

  def appendLines(lines: String*)(implicit openOptions: File.OpenOptions = File.OpenOptions.append, codec: Codec): File =
    Files.write(path, lines, codec, openOptions: _*)

  def <<(line: String)(implicit openOptions: File.OpenOptions = File.OpenOptions.append, codec: Codec): File = appendLines(line)(openOptions, codec)

  def >>:(line: String)(implicit openOptions: File.OpenOptions = File.OpenOptions.append, codec: Codec): File = appendLines(line)(openOptions, codec)

  def appendNewLine()(implicit openOptions: File.OpenOptions = File.OpenOptions.append, codec: Codec): File = appendLine("")(openOptions, codec)

  def appendLine(line: String)(implicit openOptions: File.OpenOptions = File.OpenOptions.append, codec: Codec): File = appendLines(line)(openOptions, codec)

  def append(text: String)(implicit openOptions: File.OpenOptions = File.OpenOptions.append, codec: Codec): File = append(text.getBytes(codec))(openOptions)

  def append(bytes: Array[Byte])(implicit openOptions: File.OpenOptions): File = Files.write(path, bytes, openOptions: _*)

  /**
   * Write byte array to file. For large files, piping in streams is recommended
   * @param bytes
   * @return this
   */
  def write(bytes: Array[Byte])(implicit openOptions: File.OpenOptions): File = Files.write(path, bytes, openOptions: _*)

  def write(text: String)(implicit openOptions: File.OpenOptions = File.OpenOptions.default, codec: Codec): File = write(text.getBytes(codec))(openOptions)

  def overwrite(text: String)(implicit openOptions: File.OpenOptions = File.OpenOptions.default, codec: Codec) = write(text)(openOptions, codec)

  def <(text: String)(implicit openOptions: File.OpenOptions = File.OpenOptions.default, codec: Codec) = write(text)(openOptions, codec)

  def `>:`(text: String)(implicit openOptions: File.OpenOptions = File.OpenOptions.default, codec: Codec) = write(text)(openOptions, codec)

  //TODO: @managed macro

  def newBufferedSource(implicit codec: Codec): BufferedSource = Source.fromFile(toJava)(codec)

  def bufferedSource(implicit codec: Codec): ManagedResource[BufferedSource] = newBufferedSource(codec).autoClosed

  def newRandomAccess(mode: String = "r"): RandomAccessFile = new RandomAccessFile(toJava, mode)

  def randomAccess(mode: String = "r"): ManagedResource[RandomAccessFile] = newRandomAccess(mode).autoClosed //TODO: Mode enum?

  def newBufferedReader(implicit codec: Codec): BufferedReader = Files.newBufferedReader(path, codec)

  def bufferedReader(implicit codec: Codec): ManagedResource[BufferedReader] = newBufferedReader(codec).autoClosed

  def newBufferedWriter(implicit codec: Codec, openOptions: File.OpenOptions = File.OpenOptions.default): BufferedWriter = Files.newBufferedWriter(path, codec, openOptions: _*)

  def bufferedWriter(implicit codec: Codec, openOptions: File.OpenOptions = File.OpenOptions.default): ManagedResource[BufferedWriter] = newBufferedWriter(codec, openOptions).autoClosed

  def newFileReader: FileReader = new FileReader(toJava)

  def fileReader: ManagedResource[FileReader] = newFileReader.autoClosed

  def newFileWriter(append: Boolean = false): FileWriter = new FileWriter(toJava, append)

  def fileWriter(append: Boolean = false): ManagedResource[FileWriter] = newFileWriter(append).autoClosed

  def newInputStream(implicit openOptions: File.OpenOptions = File.OpenOptions.default): InputStream = Files.newInputStream(path, openOptions: _*)

  def inputStream(implicit openOptions: File.OpenOptions = File.OpenOptions.default): ManagedResource[InputStream] = newInputStream(openOptions).autoClosed

  def newScanner(delimiter: String = File.Delimiters.default, includeDelimiters: Boolean = false)(implicit codec: Codec): Scanner = new Scanner(this, delimiter, includeDelimiters)(codec)

  def scanner(delimiter: String = File.Delimiters.default, includeDelimiters: Boolean = false)(implicit codec: Codec): ManagedResource[Scanner] = newScanner(delimiter, includeDelimiters)(codec).autoClosed

  def newOutputStream(implicit openOptions: File.OpenOptions = File.OpenOptions.default): OutputStream = Files.newOutputStream(path, openOptions: _*)

  def outputStream(implicit openOptions: File.OpenOptions = File.OpenOptions.default): ManagedResource[OutputStream] = newOutputStream(openOptions).autoClosed

  def newFileChannel(implicit openOptions: File.OpenOptions = File.OpenOptions.default, attributes: File.Attributes = File.Attributes.default): FileChannel =
    FileChannel.open(path, openOptions.toSet, attributes: _*)

  def fileChannel(implicit openOptions: File.OpenOptions = File.OpenOptions.default, attributes: File.Attributes = File.Attributes.default): ManagedResource[FileChannel] =
    newFileChannel(openOptions, attributes).autoClosed

  def newAsynchronousFileChannel(implicit openOptions: File.OpenOptions = File.OpenOptions.default): AsynchronousFileChannel = AsynchronousFileChannel.open(path, openOptions: _*)

  def asynchronousFileChannel(implicit openOptions: File.OpenOptions = File.OpenOptions.default): ManagedResource[AsynchronousFileChannel] = newAsynchronousFileChannel(openOptions).autoClosed

  def newWatchService: WatchService = fileSystem.newWatchService()

  def watchService: ManagedResource[WatchService] = newWatchService.autoClosed

  def register(service: WatchService, events: File.Events = File.Events.all): File = returning(this) {
    path.register(service, events.toArray)
  }

  def digest(algorithm: String): Array[Byte] = {
    val digestor = MessageDigest.getInstance(algorithm)
    listRelativePaths.toSeq.sorted foreach {relativePath =>
      val file = path resolve relativePath
      val bytes = if (Files.isDirectory(file)) relativePath.toString.getBytes else Files.readAllBytes(file)
      digestor.update(bytes)
    }
    digestor.digest()
  }

  /**
   * @return checksum of this file (or directory) in hex format
   */
  def checksum(algorithm: String): String = DatatypeConverter.printHexBinary(digest(algorithm))

  def md5: String = checksum("MD5")

  /**
   * @return Some(target) if this is a symbolic link (to target) else None
   */
  def symbolicLink: Option[File] = when(isSymbolicLink)(Files.readSymbolicLink(path))

  /**
   * @return true if this file (or the file found by following symlink) is a directory
   */
  def isDirectory(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): Boolean = Files.isDirectory(path, linkOptions: _*)

  /**
   * @return true if this file (or the file found by following symlink) is a regular file
   */
  def isRegularFile(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): Boolean = Files.isRegularFile(path, linkOptions: _*)

  def isSymbolicLink: Boolean = Files.isSymbolicLink(path)

  def isHidden: Boolean = Files.isHidden(path)

  def list = FilesStream(path)
  def children = list 
  def entries = list

  def listRecursively(implicit visitOptions: File.VisitOptions = File.VisitOptions.default): Files = walk()(visitOptions).filterNot(isSamePathAs)

  /**
   * Walk the directory tree recursively upto maxDepth
   * @param maxDepth
   * @return List of children in BFS maxDepth level deep (includes self since self is at depth = 0)
   */
  def walk(maxDepth: Int = Int.MaxValue)(implicit visitOptions: File.VisitOptions = File.VisitOptions.default): Files = Files.walk(path, maxDepth, visitOptions: _*) //TODO: that ignores I/O errors?

  /**
   * Util to glob from this file's path
   * @return Set of files that matched
   */
  def glob(pattern: String, syntax: String = "glob"): Files = {
    val matcher = fileSystem.getPathMatcher(s"$syntax:$pattern")
    collectChildren(child => matcher.matches(path relativize child.path))
  }

  /**
    * More Scala friendly way of doing Files.walk
    * @param f
    * @return
    */
  def collectChildren(f: File => Boolean): Files = Files.walk(path).filter(new java.util.function.Predicate[Path] {override def test(path: Path) = f(path)}) //TODO: In Scala 2.11 SAM: Files.walk(path).filter(f(_))

  def fileSystem: FileSystem = path.getFileSystem

  def uri: URI = path.toUri

  /**
   * @return file size (for directories, return size of the directory) in bytes
   */
  def size(implicit visitOptions: File.VisitOptions = File.VisitOptions.default): Long = walk()(visitOptions).map(f => Files.size(f.path)).sum

  def permissions(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): Set[PosixFilePermission] = Files.getPosixFilePermissions(path, linkOptions: _*).toSet

  def permissionsAsString(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): String = PosixFilePermissions.toString(permissions(linkOptions))

  def setPermissions(permissions: Set[PosixFilePermission]): File = Files.setPosixFilePermissions(path, permissions)

  def addPermission(permission: PosixFilePermission)(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): File = setPermissions(permissions(linkOptions) + permission)

  def removePermission(permission: PosixFilePermission)(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): File = setPermissions(permissions(linkOptions) - permission)

  /**
   * test if file has this permission
   */
  def apply(permission: PosixFilePermission)(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): Boolean = permissions(linkOptions)(permission)

  def isOwnerReadable(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): Boolean = this(PosixFilePermission.OWNER_READ)(linkOptions)

  def isOwnerWritable(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): Boolean = this(PosixFilePermission.OWNER_WRITE)(linkOptions)

  def isOwnerExecutable(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): Boolean = this(PosixFilePermission.OWNER_EXECUTE)(linkOptions)

  def isGroupReadable(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): Boolean = this(PosixFilePermission.GROUP_READ)(linkOptions)

  def isGroupWritable(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): Boolean = this(PosixFilePermission.GROUP_WRITE)(linkOptions)

  def isGroupExecutable(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): Boolean = this(PosixFilePermission.GROUP_EXECUTE)(linkOptions)

  def isOtherReadable(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): Boolean = this(PosixFilePermission.OTHERS_READ)(linkOptions)

  def isOtherWritable(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): Boolean = this(PosixFilePermission.OTHERS_WRITE)(linkOptions)

  def isOtherExecutable(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): Boolean = this(PosixFilePermission.OTHERS_EXECUTE)(linkOptions)

  /**
   * This differs from the above as this checks if the JVM can read this file even though the OS cannot in certain platforms
   * @see isOwnerReadable
   * @return
   */
  def isReadable: Boolean = toJava.canRead
  def isWriteable: Boolean = toJava.canWrite
  def isExecutable: Boolean = toJava.canExecute

  def attributes(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): BasicFileAttributes = Files.readAttributes(path, classOf[BasicFileAttributes], linkOptions: _*)

  def posixAttributes(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): PosixFileAttributes = Files.readAttributes(path, classOf[PosixFileAttributes], linkOptions: _*)

  def dosAttributes(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): DosFileAttributes   = Files.readAttributes(path, classOf[DosFileAttributes], linkOptions: _*)

  def owner(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): UserPrincipal = Files.getOwner(path, linkOptions: _*)

  def ownerName(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): String = owner(linkOptions).getName

  def group(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): GroupPrincipal = posixAttributes(linkOptions).group()

  def groupName(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): String = group(linkOptions).getName

  def setOwner(owner: String): File = Files.setOwner(path, fileSystem.getUserPrincipalLookupService.lookupPrincipalByName(owner))

  def setGroup(group: String): File = Files.setOwner(path, fileSystem.getUserPrincipalLookupService.lookupPrincipalByGroupName(group))

  /**
   * Similar to the UNIX command touch - create this file if it does not exist and set its last modification time
   */
  def touch(time: Instant = Instant.now())(implicit attributes: File.Attributes = File.Attributes.default): File =
    Files.setLastModifiedTime(createIfNotExists()(attributes).path, FileTime.from(time))

  def lastModifiedTime(implicit linkOptions: File.LinkOptions = File.LinkOptions.default): Instant = Files.getLastModifiedTime(path, linkOptions: _*).toInstant

  /**
   * Deletes this file or directory
   * @param swallowIOExceptions If this is set to true, any exception thrown is swallowed
   */
  def delete(swallowIOExceptions: Boolean = false): File = returning(this) {
    try {
      if (isDirectory) list.foreach(_.delete(swallowIOExceptions))
      Files.delete(path)
    } catch {
      case e: IOException if swallowIOExceptions => //e.printStackTrace() //swallow
    }
  }

  def renameTo(newName: String): File = moveTo(path resolveSibling newName)

  /**
   *
   * @param destination
   * @param overwrite
   * @return destination
   */
  def moveTo(destination: File, overwrite: Boolean = false): File = Files.move(path, destination.path, File.CopyOptions(overwrite): _*)

  /**
   *
   * @param destination
   * @param overwrite
   * @return destination
   */
  def copyTo(destination: File, overwrite: Boolean = false): File = if(isDirectory) { //TODO: maxDepth?
    if (overwrite) destination.delete(swallowIOExceptions = true)
    Files.walkFileTree(path, new SimpleFileVisitor[Path] {
      def newPath(subPath: Path): Path = destination.path resolve (path relativize subPath)

      override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes) = {
        Files.createDirectories(newPath(dir))
        super.preVisitDirectory(dir, attrs)
      }

      override def visitFile(file: Path, attrs: BasicFileAttributes) = {
        Files.copy(file, newPath(file), File.CopyOptions(overwrite): _*)
        super.visitFile(file, attrs)
      }
    })
    destination
  } else {
    Files.copy(path, destination.path, File.CopyOptions(overwrite): _*)
  }

  def symbolicLinkTo(destination: File)(implicit attributes: File.Attributes = File.Attributes.default) = Files.createSymbolicLink(path, destination.path, attributes: _*)

  def linkTo(destination: File, symbolic: Boolean = false)(implicit attributes: File.Attributes = File.Attributes.default): File =
    if (symbolic) symbolicLinkTo(destination)(attributes) else Files.createLink(path, destination.path)

  def listRelativePaths(implicit visitOptions: File.VisitOptions = File.VisitOptions.default): Iterator[Path] = walk()(visitOptions).map(relativize)

  def relativize(destination: File): Path = path relativize destination.path

  def isSamePathAs(that: File): Boolean = this.path == that.path

  def isSameFileAs(that: File): Boolean = Files.isSameFile(this.path, that.path)

  /**
   * @return true if this file is exactly same as that file
   *         For directories, it checks for equivalent directory structure
   */
  def isSameContentAs(that: File): Boolean = isSimilarContentAs(that)
  def `===`(that: File): Boolean = isSameContentAs(that)

  /**
   * Almost same as isSameContentAs but uses faster md5 hashing to compare (and thus small chance of false positive)
   * Also works for directories
   *
   * @param that
   * @return
   */
  def isSimilarContentAs(that: File): Boolean = this.md5 == that.md5

  def =!=(that: File): Boolean = !isSameContentAs(that)

  override def equals(obj: Any) = obj match {
    case file: File => isSamePathAs(file)
    case _ => false
  }

  /**
   * @return true if file is not present or empty directory or 0-bytes file
   */
  def isEmpty: Boolean = this match {
    case File.Type.Directory(children) => children.isEmpty
    case File.Type.RegularFile(content) => content.isEmpty
    case _ => notExists
  }

  /**
   * If this is a directory, remove all its children
   * If its a file, empty the contents
   * @return this
   */
  def clear(): File = this match {
    case File.Type.Directory(children) => children.foreach(_.delete()); this
    case _ => write(Array.ofDim[Byte](0))(File.OpenOptions.default)
  }

  override def hashCode = path.hashCode()

  override def toString = pathAsString

  /**
   * Zips this file (or directory)
   *
   * @param destination The destination file; Creates this if it does not exists
   * @return The destination zip file
   */
  def zipTo(destination: File, compressionLevel: Int = Deflater.DEFAULT_COMPRESSION)(implicit codec: Codec): File = {
    val files = if (isDirectory) children else Iterator(this)
    Cmds.zip(files.toSeq: _*)(destination, compressionLevel)(codec)
  }

  /**
   * zip to a temp directory
   * @return the target directory
   */
  def zip(compressionLevel: Int = Deflater.DEFAULT_COMPRESSION)(implicit codec: Codec): File = zipTo(destination = File.newTemporaryFile(name, ".zip"), compressionLevel)(codec)

  /**
   * Unzips this zip file
   *
   * @param destination destination folder; Creates this if it does not exist
   * @return The destination where contents are unzipped
   */
  def unzipTo(destination: File)(implicit codec: Codec): File = returning(destination) {
    for {
      zipFile <- new ZipFile(toJava, codec).autoClosed
      entry <- zipFile.entries()
      file = destination.createChild(entry.getName, entry.isDirectory)
      if !entry.isDirectory
    } zipFile.getInputStream(entry) > file.newOutputStream
  }

  /**
   * unzip to a temporary zip file
   * @return the zip file
   */
  def unzip()(implicit codec: Codec): File = unzipTo(destination = File.newTemporaryDirectory(name))(codec)

  //TODO: add features from https://github.com/sbt/io/blob/master/io/src/main/scala/sbt/io/IO.scala
}

object File {
  def newTemporaryDirectory(prefix: String = "", parent: Option[File] = None)(implicit attributes: Attributes = Attributes.default): File =
    parent match {
      case Some(dir) => Files.createTempDirectory(dir.path, prefix, attributes: _*)
      case _ => Files.createTempDirectory(prefix, attributes: _*)
    }

  def newTemporaryFile(prefix: String = "", suffix: String = "", parent: Option[File] = None)(implicit attributes: Attributes = Attributes.default): File =
    parent match {
      case Some(dir) => Files.createTempFile(dir.path, prefix, suffix, attributes: _*)
      case _ => Files.createTempFile(prefix, suffix, attributes: _*)
    }

  def apply(path: String, fragments: String*): File = Paths.get(path, fragments: _*)

  def apply(uri: URI): File = Paths.get(uri)

  def roots: Iterable[File] = FileSystems.getDefault.getRootDirectories.map(p => new File(p))

  def root: File = roots.head

  def home: File = Properties.userHome.toFile

  def temp: File = Properties.tmpDir.toFile

  def currentWorkingDirectory: File = File("")

  type Attributes = Seq[FileAttribute[_]]
  object Attributes {
    val default   : Attributes = Seq.empty
  }

  type CopyOptions = Seq[StandardCopyOption]
  object CopyOptions {
    def apply(overwrite: Boolean) : CopyOptions = if (overwrite) StandardCopyOption.REPLACE_EXISTING +: default else default
    val default                   : CopyOptions = Seq.empty //Seq(StandardCopyOption.COPY_ATTRIBUTES)
  }

  type Delimiters = String
  object Delimiters {
    val spaces  : Delimiters = " \t\n\r\f"
    val default : Delimiters = spaces
  }

  type Events = Seq[WatchEvent.Kind[_]]
  object Events {
    val all     : Events = Seq(StandardWatchEventKinds.ENTRY_CREATE, StandardWatchEventKinds.ENTRY_MODIFY, StandardWatchEventKinds.ENTRY_DELETE)
    val default : Events = all
  }

  type OpenOptions = Seq[OpenOption]
  object OpenOptions {
    val append  : OpenOptions = Seq(StandardOpenOption.APPEND, StandardOpenOption.CREATE)
    val default : OpenOptions = Seq.empty
  }

  type LinkOptions = Seq[LinkOption]
  object LinkOptions {
    val follow    : LinkOptions = Seq.empty
    val noFollow  : LinkOptions = Seq(LinkOption.NOFOLLOW_LINKS)
    val default   : LinkOptions = follow
  }

  type VisitOptions = Seq[FileVisitOption]
  object VisitOptions {
    val follow    : VisitOptions = Seq(FileVisitOption.FOLLOW_LINKS)
    val default   : VisitOptions = Seq.empty
  }

  type Order = Ordering[File]
  object Order {
    val bySize              : Order = Ordering.by(_.size)
    val byName              : Order = Ordering.by(_.name)
    val byDepth             : Order = Ordering.by(_.path.getNameCount)
    val byModificationTime  : Order = Ordering.by(_.lastModifiedTime)
    val byDirectoriesLast   : Order = Ordering.by(_.isDirectory)
    val byDirectoriesFirst  : Order = byDirectoriesLast.reverse
    val default             : Order = byDirectoriesFirst
  }

  /**
   * Denote various file types using this
   *
   * @tparam Content The type of underlying contents e.g. a directory has its children files as contents but a regular file may have bytes as contents
   */
  sealed trait Type[Content] {
    def unapply(file: File): Option[Content]
  }

  object Type {
    case object RegularFile extends Type[BufferedSource] {
      override def unapply(file: File) = when(file.isRegularFile)(file.newBufferedSource)
    }

    case object Directory extends Type[Files] {
      def unapply(file: File) = when(file.isDirectory)(file.children)
    }

    case object SymbolicLink extends Type[File] {
      def unapply(file: File) = file.symbolicLink
    }
  }

  /**
    * Implement this interface to monitor the root file
    */
  trait Monitor {
    val root: File

    /**
      * Dispatch a StandardWatchEventKind to an appropriate callback
      * Override this if you don't want to manually handle onDelete/onCreate/onModify separately
      *
      * @param eventType
      * @param file
      */
    def onEvent(eventType: WatchEvent.Kind[Path], file: File): Unit = eventType match {
      case StandardWatchEventKinds.ENTRY_CREATE => onCreate(file)
      case StandardWatchEventKinds.ENTRY_MODIFY => onModify(file)
      case StandardWatchEventKinds.ENTRY_DELETE => onDelete(file)
    }

    def start(): Unit

    def onCreate(file: File): Unit

    def onModify(file: File): Unit

    def onDelete(file: File): Unit

    def onUnknownEvent(event: WatchEvent[_]): Unit

    def onException(exception: Throwable): Unit

    def stop(): Unit
  }
}
