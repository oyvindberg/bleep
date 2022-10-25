package bleep.logging

sealed abstract class LogLevel(val level: Int)(implicit val name: sourcecode.Name)

object LogLevel {
  case object debug extends LogLevel(2)
  case object info extends LogLevel(3)
  case object warn extends LogLevel(4)
  case object error extends LogLevel(5)

  implicit val LogLevelOrdering: Ordering[LogLevel] = (one, two) => one.level.compareTo(two.level)

  val All = List(debug, info, warn, error)

  def unsafeFrom(level: Int): LogLevel =
    if (level == debug.level) debug
    else if (level == info.level) info
    else if (level == warn.level) warn
    else if (level == error.level) error
    else sys.error(s"unexpected logLevel $level")

}
