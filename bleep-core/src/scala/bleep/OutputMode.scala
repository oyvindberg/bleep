package bleep

sealed trait OutputMode {

  /** True when stdout should be reserved for machine-readable output (logs go to stderr). */
  def stdoutIsMachineReadable: Boolean = this match {
    case OutputMode.Text => false
    case OutputMode.Json => true
    case OutputMode.Raw  => true
  }
}
object OutputMode {
  case object Text extends OutputMode
  case object Json extends OutputMode

  /** Raw mode: logs to stderr, plain values to stdout (one per line, no JSON wrapping). */
  case object Raw extends OutputMode
}
