package better.files

import java.io.{File => JFile, _}, StreamTokenizer.{TT_EOF => eof}
import java.nio.MappedByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.Charset
import java.nio.file.Path
import java.util.StringTokenizer
import java.util.stream.{Stream => JStream}
import java.util.zip.{Deflater, GZIPInputStream, ZipEntry, ZipOutputStream, GZIPOutputStream}

import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.io.{BufferedSource, Codec, Source}

/**
 * Container for various implicits
 */
trait Implicits {
  implicit class StringInterpolations(sc: StringContext) {
    def file(args: Any*): File = value(args).toFile
    def resource(args: Any*): Source = Source.fromInputStream(getClass.getResourceAsStream(value(args)))
    private[this] def value(args: Seq[Any]) = sc.s(args: _*)
  }

  implicit class StringOps(str: String) {
    def toFile: File = File(str)
    def /(child: String): File = toFile/child
  }

  implicit class FileOps(file: JFile) {
    def toScala: File = File(file.getPath)
  }

  implicit class InputStreamOps(in: InputStream) {
    def >(out: OutputStream): Unit = pipeTo(out)

    def pipeTo(out: OutputStream, closeOutputStream: Boolean = true, bufferSize: Int = 1<<10): Unit = pipeTo(out, closeOutputStream, Array.ofDim[Byte](bufferSize))

    /**
     * Pipe an input stream to an output stream using a byte buffer
     */
    @tailrec final def pipeTo(out: OutputStream, closeOutputStream: Boolean, buffer: Array[Byte]): Unit = in.read(buffer) match {
      case n if n > 0 =>
        out.write(buffer, 0, n)
        pipeTo(out, closeOutputStream, buffer)
      case _ =>
        in.close()
        if(closeOutputStream) out.close()
    }

    def buffered: BufferedInputStream = new BufferedInputStream(in)

    def gzipped: GZIPInputStream = new GZIPInputStream(in)

    def reader(implicit codec: Codec): InputStreamReader = new InputStreamReader(in, codec)

    def content(implicit codec: Codec): BufferedSource = Source.fromInputStream(in)(codec)

    def lines(implicit codec: Codec): Iterator[String] = content(codec).getLines()

    def bytes: Iterator[Byte] = in.autoClosedIterator(_.read())(_ != eof).map(_.toByte)
  }

  implicit class OutputStreamOps(out: OutputStream) {
    def buffered: BufferedOutputStream = new BufferedOutputStream(out)

    def gzipped: GZIPOutputStream = new GZIPOutputStream(out)

    def writer(implicit codec: Codec): OutputStreamWriter = new OutputStreamWriter(out, codec)

    def printer(autoFlush: Boolean = false): PrintWriter = new PrintWriter(out, autoFlush)
  }

  implicit class ReaderOps(reader: Reader) {
    def buffered: BufferedReader = new BufferedReader(reader)
  }

  implicit class BufferedReaderOps(reader: BufferedReader) {
    def chars: Iterator[Char] = reader.autoClosedIterator(_.read())(_ != eof).map(_.toChar)
  }

  implicit class WriterOps(writer: Writer) {
    def buffered: BufferedWriter = new BufferedWriter(writer)
  }

  implicit class FileChannelOps(fc: FileChannel) {
    def toMappedByteBuffer: MappedByteBuffer = fc.map(FileChannel.MapMode.READ_ONLY, 0, fc.size())
  }

  implicit class ZipOutputStreamOps(out: ZipOutputStream) {

    /**
     * Correctly set the compression level
     * See: http://stackoverflow.com/questions/1206970/creating-zip-using-zip-utility
     *
     * @param level
     * @return
     */
    def withCompressionLevel(level: Int) = returning(out) {
      out.setLevel(level)
      if(level == Deflater.NO_COMPRESSION) out.setMethod(ZipOutputStream.DEFLATED)
    }

    def add(file: File, name: String): ZipOutputStream = returning(out) {
      val relativeName = name.stripSuffix(file.fileSystem.getSeparator)
      val entryName = if (file.isDirectory) s"$relativeName/" else relativeName // make sure to end directories in ZipEntry with "/"
      out.putNextEntry(new ZipEntry(entryName))
      if (file.isRegularFile) file.newInputStream.pipeTo(out, closeOutputStream = false)
      out.closeEntry()
    }

    def +=(file: File): ZipOutputStream = add(file, file.name)
  }

  implicit class CloseableOps[A <: Closeable](resource: A) {
    import scala.language.reflectiveCalls
    /**
     * Overrides `foreach` such that it closes the resource
     * when done, or if an exception occurs.
		 *
     * e.g.
     * <pre>
     * for {
     *   in <- file.newInputStream.autoClosed
     * } in.write(bytes)
     * // in is closed now
     * </pre>
     * @return
     * 
     * FIXME: there is no reason to swallow all exceptions here.
     */
    def autoClosed: ManagedResource[A] = new Traversable[A] {
      override def foreach[U](f: A => U) = try {
        f(resource)
      } finally {
        resource.close()
      }
    }

    /**
     * Provides and iterator that closes the underlying resource
     * when done.
     * 
     * e.g.
     * <pre>
     * inputStream.autoClosedIterator(_.read())(_ != -1).map(_.toByte)
     * </pre>
     *
     * @param next next element in this resource
     * @param hasNext a function which tells if there is no more B left
     * @tparam B
     * @return An iterator that closes itself when done
     */
    def autoClosedIterator[B](next: A => B)(hasNext: B => Boolean): Iterator[B] = {
      var isClosed = false
      def isOpen(item: B) = {
        if(!isClosed && !hasNext(item)) {
          resource.close()
          isClosed = true
        }
        !isClosed
      }
      Iterator.continually(next(resource)).takeWhile(isOpen)
    }
  }

  implicit def tokenizerToIterator(s: StringTokenizer): Iterator[String] = new Iterator[String] {
    override def hasNext = s.hasMoreTokens
    override def next() = s.nextToken()
  }

  implicit def codecToCharSet(codec: Codec): Charset = codec.charSet

  //implicit def posixPermissionToFileAttribute(perm: PosixFilePermission) = PosixFilePermissions.asFileAttribute(Set(perm))

  implicit def pathToFile(path: Path): File = new File(path)
  private[files] implicit def pathStreamToFiles(files: JStream[Path]): Files = files.iterator().map(pathToFile)
}
