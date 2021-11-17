package bleep

abstract class BuildException(
    val message: String,
    cause: Throwable = null
) extends Exception(message, cause)

object BuildException {
  final class ModuleFormatError(
      val moduleString: String,
      val error: String,
  ) extends BuildException(s"Error parsing module '$moduleString': $error")

  final class Cause(
      val cause: Throwable,
      val error: String,
  ) extends BuildException(error, cause)
}
