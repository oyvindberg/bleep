package bleep
package commands

import bleep.BleepException
import bleep.bsp.BleepRifleLogger
import bleep.internal.FileUtils
import bleep.logging.Logger

import java.io.OutputStream
import java.nio.file.{Files, Path}
import scala.build.blooprifle.BloopRifleConfig
import scala.build.blooprifle.internal.Operations
import scala.jdk.StreamConverters.StreamHasToScala

case class CompileServerStopAll(logger: Logger, userPaths: UserPaths) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    val socketDirs: List[Path] =
      if (FileUtils.exists(userPaths.bspSocketDir)) Files.list(userPaths.bspSocketDir).toScala(List)
      else Nil

    val rifleLogger = new BleepRifleLogger(logger)

    socketDirs.foreach { socketDir =>
      val address = BloopRifleConfig.Address.DomainSocket(socketDir)
      if (Operations.check(address, rifleLogger)) {
        logger.info(s"stopping bloop server running at socket $socketDir")
        Operations.exit(
          address = address,
          workingDir = FileUtils.TempDir,
          out = rifleLogger.bloopBspStdout.getOrElse(OutputStream.nullOutputStream()),
          err = rifleLogger.bloopBspStderr.getOrElse(OutputStream.nullOutputStream()),
          logger = rifleLogger
        )
        FileUtils.deleteDirectory(socketDir)
      } else
        logger.info(s"bloop server was not running at socket $socketDir")
    }
    Right(())
  }
}
