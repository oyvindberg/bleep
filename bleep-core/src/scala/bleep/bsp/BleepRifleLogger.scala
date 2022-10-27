package bleep.bsp

import bleep.logging.{jsonEvents, Logger, LoggerFn}

import java.io.OutputStream
import java.nio.charset.StandardCharsets
import scala.build.blooprifle.BloopRifleLogger

class BleepRifleLogger(logger: Logger) extends BloopRifleLogger {
  val bloopLogger = logger.withPath("bloop")
  val bloopRifleLogger = logger.withPath("bloop-rifle")

  override def info(msg: => String): Unit =
    io.circe.parser.decode[jsonEvents.JsonEvent](msg) match {
      case Left(_)          => bloopRifleLogger.info(msg)
      case Right(jsonEvent) => jsonEvent.logTo(logger)
    }

  override def debug(msg: => String, throwable: Throwable): Unit =
    Option(throwable) match {
      case Some(th) => bloopRifleLogger.debug(msg, th)
      case None     => bloopRifleLogger.debug(msg)
    }

  override def error(msg: => String, ex: Throwable): Unit = bloopRifleLogger.error(msg)
  override val bloopCliInheritStdout: Boolean = false
  override val bloopCliInheritStderr: Boolean = false
  override def bloopBspStdout: Option[OutputStream] = Some(new BleepRifleLogger.Stream(bloopLogger))
  override def bloopBspStderr: Option[OutputStream] = Some(new BleepRifleLogger.Stream(bloopLogger))
}

object BleepRifleLogger {
  private class Stream(logger: LoggerFn) extends OutputStream {
    val bs = Array.newBuilder[Byte]
    override def write(b: Int): Unit = {
      val NewLine = '\n'.toByte
      b.toByte match {
        case NewLine =>
          val line = new String(bs.result(), StandardCharsets.UTF_8)

          fansi.Str.Strip(line).plainText.splitAt(4) match {
            case ("[E] ", rest) => logger.error(rest)
            case ("[W] ", rest) => logger.warn(rest)
            case ("[I] ", rest) => logger.info(rest)
            case ("[D] ", rest) => logger.debug(rest)
            case _              => logger.debug(line)
          }

          bs.clear()
        case other =>
          bs += other
      }
    }
  }
}
