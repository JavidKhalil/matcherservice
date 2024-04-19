package mathcer.util

import java.io.{BufferedWriter, FileWriter}
import java.nio.charset.Charset
import scala.io.{BufferedSource, Source}

/**
 * File utils
 *
 */

// Define a trait for FileUtil functionality
trait FileUtil {
  // Define a type alias for the service
  type FileUtils = Service

  // Define the service trait
  trait Service {
    def readLines(path: String): List[Array[String]]
    def writeFile(path: String, content: String): Unit
  }
}

// Define the companion object for FileUtil
object FileUtil extends FileUtil {
  // Implement the service trait
  object FileServiceImpl extends Service {
    def readLines(path: String): List[Array[String]] = {
      val source = Source.fromFile(path)(Charset.defaultCharset())
      val lines = source.getLines().toList.map(_.split("\\s+"))
      source.close()
      lines
    }

    def writeFile(path: String, content: String): Unit = {
      val writer = new BufferedWriter(new FileWriter(path))
      writer.write(content)
      writer.close()
    }
  }

  // Export the service as a val
  val fileService: Service = FileServiceImpl
}