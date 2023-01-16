package bleep

sealed abstract class ExitCode(val value: Int) {
  final def andThen(f: => ExitCode): ExitCode =
    this match {
      case ExitCode.Success => f
      case ExitCode.Failure => this
    }
}

object ExitCode {
  case object Success extends ExitCode(0)
  case object Failure extends ExitCode(1)
}
