package bleep
package commands

import bleep.bsp.{BspRifleConfig, BspServerOperations}
import bleep.internal.FileUtils
import cats.effect.unsafe.implicits.global
import ryddig.Logger

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.jdk.StreamConverters.StreamHasToScala

case class CompileServerStopAll(logger: Logger, userPaths: UserPaths) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    val socketDirs: List[Path] =
      if (FileUtils.exists(userPaths.bspSocketDir)) Files.list(userPaths.bspSocketDir).toScala(List)
      else Nil

    socketDirs.foreach { socketDir =>
      val socketPath = socketDir.resolve("socket")
      val address = BspRifleConfig.Address.DomainSocket(socketPath)

      val isRunning = BspServerOperations.check(address).unsafeRunSync()
      if (isRunning) {
        logger.info(s"stopping bleep-bsp server running at socket $socketDir")
        BspServerOperations.forceKillAndCleanup(socketDir).unsafeRunSync()
      } else {
        logger.info(s"bleep-bsp server was not running at socket $socketDir")
      }

      // Give the OS time to release file handles after process kill
      Thread.sleep(200)
      val sizeMb = dirSizeBytes(socketDir) / (1024L * 1024L)
      try FileUtils.deleteDirectory(socketDir)
      catch {
        case _: java.nio.file.DirectoryNotEmptyException | _: java.nio.file.FileSystemException =>
          // Retry once after a longer pause — killed process may still be releasing files
          // On Windows, locked files throw FileSystemException instead of DirectoryNotEmptyException
          Thread.sleep(2000)
          FileUtils.deleteDirectory(socketDir)
      }
      // These dirs can hold multi-GB diagnostic artifacts (heap dumps from pre-M11 servers, output
      // logs). Deleting gigabytes without a word once cost a user the evidence for an OOM report —
      // say what went away, and stand out when it was big.
      val msg = s"deleted $socketDir (${sizeMb}MB)"
      if (sizeMb >= 1024) logger.warn(msg) else logger.info(msg)
    }
    Right(())
  }

  private def dirSizeBytes(dir: Path): Long = {
    val stream = Files.walk(dir)
    try stream.iterator().asScala.filter(Files.isRegularFile(_)).map(Files.size).sum
    finally stream.close()
  }
}
