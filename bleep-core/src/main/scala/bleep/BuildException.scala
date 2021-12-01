package bleep

import coursier.error.CoursierError
import io.circe

import java.nio.file.Path

abstract class BuildException(
    val message: String,
    cause: Throwable = null
) extends Exception(message, cause)

object BuildException {
  class BuildNotFound(cwd: Path) extends BuildException(s"Couldn't find ${Defaults.BuildFileName} in directories in or above $cwd")

  class InvalidJson(file: Path, e: circe.Error) extends BuildException(s"Couldn't parse json file $file", e)

  class ModuleFormatError(
      val moduleString: String,
      val error: String
  ) extends BuildException(s"Error parsing module '$moduleString': $error")

  class ResolveError(
      cause: CoursierError,
      context: String
  ) extends BuildException(s"Error resolving dependencies for $context", cause)

  object ResolveError {
    def apply(cause: CoursierError, project: model.ProjectName): ResolveError = new ResolveError(cause, s"project ${project.value}")
  }

  class Cause(
      val cause: Throwable,
      val error: String
  ) extends BuildException(error, cause)
}
