package bleep.bsp

import bleep.logging.{Logger, LoggerFn}

import java.io.OutputStream
import java.nio.charset.StandardCharsets
import scala.build.blooprifle.BloopRifleLogger

class BleepRifleLogger(logger: Logger) extends BloopRifleLogger {
  override def info(msg: => String): Unit = logger.info(s"bloop-rifle: $msg")
  override def debug(msg: => String, throwable: Throwable): Unit =
    Option(throwable) match {
      case Some(th) => logger.debug(s"bloop-rifle: $msg", th)
      case None     => logger.debug(s"bloop-rifle: $msg")
    }

  override def error(msg: => String, ex: Throwable): Unit = logger.error(s"bloop-rifle: $msg")
  override val bloopCliInheritStdout: Boolean = false
  override val bloopCliInheritStderr: Boolean = false
  override def bloopBspStdout: Option[OutputStream] = Some(new BleepRifleLogger.Log("bloop: ", logger))
  override def bloopBspStderr: Option[OutputStream] = Some(new BleepRifleLogger.Log("bloop: ", logger))
}

object BleepRifleLogger {
  private class Log(prefix: String, logger: LoggerFn) extends OutputStream {
    val bs = Array.newBuilder[Byte]
    override def write(b: Int): Unit = {
      val NewLine = '\n'.toByte
      b.toByte match {
        case NewLine =>
          val line = new String(bs.result(), StandardCharsets.UTF_8)

          fansi.Str.Strip(line).plainText.splitAt(4) match {
            case ("[E] ", rest) => logger.error(prefix + rest)
            case ("[W] ", rest) => logger.warn(prefix + rest)
            case ("[I] ", rest) => logger.info(prefix + rest)
            case ("[D] ", rest) => logger.debug(prefix + rest)
            case _              => logger.debug(prefix + line)
          }

          bs.clear()
        case other =>
          bs += other
      }
    }
  }
}
