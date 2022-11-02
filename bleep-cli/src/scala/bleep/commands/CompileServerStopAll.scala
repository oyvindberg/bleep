package bleep
package commands

import bleep.bsp.{BleepRifleLogger, SetupBloopRifle}
import bleep.internal.FileUtils
import bleep.logging.Logger
import bleep.{BleepException, Lazy}

import java.io.OutputStream
import java.nio.file.{Files, Path}
import scala.build.blooprifle.BloopRifleConfig
import scala.build.blooprifle.internal.Operations
import scala.jdk.StreamConverters.StreamHasToScala

case class CompileServerStopAll(logger: Logger, userPaths: UserPaths) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    val socketDirs: List[Path] =
      if (FileUtils.exists(userPaths.bspSocketDir))
        Files.list(userPaths.bspSocketDir).filter(_.getFileName.toString.contains(SetupBloopRifle.SharedPrefix)).toScala(List)
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
      } else
        logger.info(s"bloop server was not running at socket $socketDir")
    }
    Right(())
  }
}
