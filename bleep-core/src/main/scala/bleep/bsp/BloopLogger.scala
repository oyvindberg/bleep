package bleep.bsp

import bleep.logging.Logger

import java.io.OutputStream
import java.nio.charset.StandardCharsets
import scala.build.blooprifle.BloopRifleLogger

class BloopLogger(logger: Logger) extends BloopRifleLogger {
  override def info(msg: => String): Unit = logger.info(s"bloop: $msg")
  override def debug(msg: => String): Unit = logger.debug(s"bloop: $msg")
  override def error(msg: => String, ex: Throwable): Unit = logger.error(s"bloop: $msg")
  override val bloopCliInheritStdout: Boolean = false
  override val bloopCliInheritStderr: Boolean = false
  override def bloopBspStdout: Option[OutputStream] = Some(new BloopLogger.Log(debug(_)))
  override def bloopBspStderr: Option[OutputStream] = Some(new BloopLogger.Log(info(_)))
}

object BloopLogger {
  private class Log(log: String => Unit) extends OutputStream {
    val bs = Array.newBuilder[Byte]
    override def write(b: Int): Unit = {
      val NewLine = '\n'.toByte
      b.toByte match {
        case NewLine =>
          log(new String(bs.result(), StandardCharsets.UTF_8))
          bs.clear()
        case other =>
          bs += other
      }
    }
  }
}
