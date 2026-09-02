package bleep

sealed abstract class ExitCode(val value: Int) {
  final def andThen(f: => ExitCode): ExitCode =
    if (value == 0) f else this
}

object ExitCode {
  case object Success extends ExitCode(0)
  case object Failure extends ExitCode(1)

  /** A program bleep launched ran to completion and chose this code, so bleep exits with it too. See [[BleepException.SubprocessExit]]. */
  case class FromSubprocess(code: Int) extends ExitCode(code)
}
