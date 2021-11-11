package bleep

import bleep.Logger.Level

trait Logger {
  def log(level: Level, message: => String): Unit
  def trace(th: Throwable): Unit

  final def verbose(message: => String): Unit = debug(message)

  final def debug(message: => String): Unit = log(Level.Debug, message)

  final def info(message: => String): Unit = log(Level.Info, message)

  final def warn(message: => String): Unit = log(Level.Warn, message)

  final def error(message: => String): Unit = log(Level.Error, message)

  final def success(message: => String): Unit = log(Level.Success, message)

}

object Logger {
  object Println extends Logger {
    override def log(level: Level, message: => String): Unit = println(s"${level.name.padTo(5, ' ')}: $message")

    override def trace(th: Throwable): Unit = th.printStackTrace()
  }

  sealed abstract class Level(val name: String)
  object Level {
    object Debug extends Level("debug")
    object Info extends Level("info")
    object Warn extends Level("warn")
    object Error extends Level("error")
    object Success extends Level("success")
  }
}
