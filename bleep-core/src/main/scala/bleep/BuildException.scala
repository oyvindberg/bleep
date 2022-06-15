package bleep

import bleep.internal.throwableMessages
import bleep.logging.Logger
import coursier.cache.ArtifactError
import coursier.error.CoursierError

import java.nio.file.Path

abstract class BuildException(
    val message: String,
    cause: Throwable = null
) extends Exception(message, cause)

object BuildException {
  def fatal(context: String, logger: Logger, throwable: Throwable): Nothing = {
    throwable match {
      case buildException: BuildException =>
        logger.debug(context, buildException)
        logger.error(throwableMessages(buildException).mkString(": "))
      case unexpected =>
        logger.error(context, unexpected)
    }
    sys.exit(1)
  }

  class BuildNotFound(cwd: Path) extends BuildException(s"Couldn't find ${constants.BuildFileName} in directories in or above $cwd")

  class TargetFolderNotDetermined(projectName: model.CrossProjectName)
      extends BuildException(s"Couldn't determine original output directory of project ${projectName.name}")

  class InvalidJson(file: Path, e: Throwable) extends BuildException(s"Couldn't parse json file $file", e)

  class ModuleFormatError(
      val moduleString: String,
      val error: String
  ) extends BuildException(s"Error parsing module '$moduleString': $error")

  class ResolveError(
      cause: CoursierError,
      context: String
  ) extends BuildException(s"Error resolving dependencies for $context", cause)

  class ArtifactResolveError(
      cause: ArtifactError,
      context: String
  ) extends BuildException(s"Error resolving artifact for $context", cause)

  object ResolveError {
    def apply(cause: CoursierError, project: model.CrossProjectName): ResolveError = new ResolveError(cause, s"project ${project.value}")
  }

  class Text(maybeProject: Option[model.CrossProjectName], str: String)
      extends BuildException(maybeProject match {
        case Some(project) => s"${project.value}: $str"
        case None          => str
      }) {
    def this(str: String) = this(None, str)
    def this(project: model.CrossProjectName, str: String) = this(Some(project), str)
  }

  class Cause(
      val cause: Throwable,
      val error: String
  ) extends BuildException(error, cause)
}
