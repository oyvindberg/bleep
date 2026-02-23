package bleep
package commands

import bleep.bsp.{BspRifleConfig, BspServerOperations}
import bleep.internal.FileUtils
import cats.effect.unsafe.implicits.global
import ryddig.Logger

import java.nio.file.{Files, Path}
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
      try FileUtils.deleteDirectory(socketDir)
      catch {
        case _: java.nio.file.DirectoryNotEmptyException =>
          // Retry once after a longer pause — killed process may still be releasing files
          Thread.sleep(500)
          FileUtils.deleteDirectory(socketDir)
      }
    }
    Right(())
  }
}
