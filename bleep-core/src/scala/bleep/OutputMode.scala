package bleep

sealed trait OutputMode
object OutputMode {
  case object Text extends OutputMode
  case object Json extends OutputMode
}
