package bleep.nosbt.util

import java.util.function.Supplier

abstract class Logger {
  final def verbose(message: => String): Unit = debug(message)
  final def debug(message: => String): Unit = log(Level.Debug, message)
  final def info(message: => String): Unit = log(Level.Info, message)
  final def warn(message: => String): Unit = log(Level.Warn, message)
  final def error(message: => String): Unit = log(Level.Error, message)

  // Added by sys.process.ProcessLogger
  final def err(message: => String): Unit = log(Level.Error, message)
  // sys.process.ProcessLogger
  final def out(message: => String): Unit = log(Level.Info, message)

  @deprecated("No longer used.", "1.0.0")
  def ansiCodesSupported: Boolean = false

  def trace(t: => Throwable): Unit
  def success(message: => String): Unit
  def log(level: Level.Value, message: => String): Unit

  def debug(msg: Supplier[String]): Unit = log(Level.Debug, msg)
  def warn(msg: Supplier[String]): Unit = log(Level.Warn, msg)
  def info(msg: Supplier[String]): Unit = log(Level.Info, msg)
  def error(msg: Supplier[String]): Unit = log(Level.Error, msg)
  def trace(msg: Supplier[Throwable]): Unit = trace(msg.get())
  def log(level: Level.Value, msg: Supplier[String]): Unit = log(level, msg.get)
}
