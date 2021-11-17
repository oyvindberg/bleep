package bleep.internal

import bleep.Logger

import scala.build.blooprifle.BloopRifleLogger

class MyBloopRifleLogger(logger: Logger, val bloopCliInheritStdout: Boolean, val bloopCliInheritStderr: Boolean) extends BloopRifleLogger {
  override def info(msg: => String): Unit = logger.info(s"bloop: $msg")
  override def debug(msg: => String): Unit = logger.debug(s"bloop: $msg")
  override def error(msg: => String, ex: Throwable): Unit = logger.error(s"bloop: $msg")
}
