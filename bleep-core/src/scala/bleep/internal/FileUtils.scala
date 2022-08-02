package bleep
package internal

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes

object FileUtils {
  lazy val TempDir = Path.of(System.getProperty("java.io.tmpdir"))

  def writeBytes(path: Path, newContent: Array[Byte]): Unit = {
    Files.createDirectories(path.getParent)
    Files.write(path, newContent, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
    ()
  }

  def writeString(path: Path, newContent: String): Unit =
    writeBytes(path, newContent.getBytes(StandardCharsets.UTF_8))

  def deleteDirectory(dir: Path): Unit =
    if (FileUtils.exists(dir)) {
      Files.walkFileTree(
        dir,
        new SimpleFileVisitor[Path] {
          override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
            Files.deleteIfExists(file)
            FileVisitResult.CONTINUE
          }

          override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
            Files.deleteIfExists(dir)
            FileVisitResult.CONTINUE
          }
        }
      )
      ()
    }

  // Files.exists is too slow because it throws exceptions behind the scenes
  def exists(path: Path): Boolean = path.toFile.exists()
}
