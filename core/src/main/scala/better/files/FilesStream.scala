package better.files
import java.util.stream.{Stream => JStream}
import scala.collection.mutable.HashMap
import java.lang.management.ManagementFactory
import java.lang.management.OperatingSystemMXBean
import com.sun.management.UnixOperatingSystemMXBean

/*
 * Provides an iterator that auto-closes the underlying java
 * stream, to avoid leaking open file descriptors.
 * 
 * If the stream is not depleted, it will not auto-close.
 */
object FilesStream extends {
  
  def apply(path: java.nio.file.Path)(implicit trace: Boolean = false, audit: Boolean = true): Iterator[File] = {
    
    val jstream: JStream[java.nio.file.Path] = java.nio.file.Files.list(path)
    if (audit) StreamLeakAudit("open", path.toString, trace)
    
    val streamIterator = jstream.iterator
  
    def close() = {
      jstream.close
      if (audit) StreamLeakAudit("close", path.toString, trace)
    }
    
    def get: Option[File] = {
      streamIterator.hasNext match {
        case false =>
          close 
          None
        case true =>
          val item = Some(new File(streamIterator.next))
          item
      }
    }
   
    /* the iterator facade returned for the underlying directory stream */
    Iterator.continually(get).takeWhile(_.isDefined).map(_.get) //
  }
}

/*
 * stream leakage tracking, non-thread-safe object
 */
object StreamLeakAudit {
   
  private var streams: HashMap[String, String] = HashMap()
  
  private[files] def apply(state: String, pathString: String, trace: Boolean = false) = {
    if (trace) println(s"files stream ${state}ed for path: $pathString")
    streams.put(pathString, state)
  }
  
  def getOpenStreams: Iterable[String] = {
    streams.collect { case (pathString, "open") => pathString } 
  }
  
  def reset() = {
    streams = HashMap()
  }
}

object WithLeakAccounting {
  
  def apply[T](block: => T) = { // for syntax clarification: http://stackoverflow.com/a/22670623/1509695 
    
    val initialOpenDescriptors = OpenFilesCount.get
    StreamLeakAudit.reset
    
    val returnVal = block
    
    val fileLeak = OpenFilesCount.get - initialOpenDescriptors
    val streamLeak = StreamLeakAudit.getOpenStreams.size
    
    if (fileLeak > 0) {
      print(Console.YELLOW + Console.BOLD)
      println(s"leak ― file descriptors increase:  $fileLeak")
      print(Console.RESET)
    }
    
    if (streamLeak > 0) {
      print(Console.YELLOW + Console.BOLD)
      println(s"leak ― unclosed directory streams: $streamLeak. unclosed streams:")
      println(StreamLeakAudit.getOpenStreams.mkString("\n"))
      print(Console.RESET)
    }
    returnVal
  }
}

object OpenFilesCount {
  def get: Long = {
    val os = ManagementFactory.getOperatingSystemMXBean
      if(!os.isInstanceOf[UnixOperatingSystemMXBean]) throw new Exception("method called only supports Unix")
      os.asInstanceOf[UnixOperatingSystemMXBean].getOpenFileDescriptorCount
  }
}