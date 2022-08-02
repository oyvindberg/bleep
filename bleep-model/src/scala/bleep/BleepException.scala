package bleep

import coursier.cache.ArtifactError
import coursier.error.CoursierError

import java.nio.file.Path

abstract class BleepException(
    val message: String,
    cause: Throwable = null
) extends Exception(message, cause)

object BleepException {
  class BuildNotFound(cwd: Path) extends BleepException(s"Couldn't find ${BuildLoader.BuildFileName} in directories in or above $cwd")

  class TargetFolderNotDetermined(projectName: model.CrossProjectName)
      extends BleepException(s"Couldn't determine original output directory of project ${projectName.name}")

  class InvalidJson(file: Path, e: Throwable) extends BleepException(s"Couldn't parse json file $file", e)

  class ModuleFormatError(
      val moduleString: String,
      val error: String
  ) extends BleepException(s"Error parsing module '$moduleString': $error")

  class ResolveError(
      cause: CoursierError,
      context: String
  ) extends BleepException(s"Error resolving dependencies for $context", cause)

  class ArtifactResolveError(
      cause: ArtifactError,
      context: String
  ) extends BleepException(s"Error resolving artifact for $context", cause)

  object ResolveError {
    def apply(cause: CoursierError, project: model.CrossProjectName): ResolveError = new ResolveError(cause, s"project ${project.value}")
  }

  class Text(maybeProject: Option[model.CrossProjectName], str: String)
      extends BleepException(maybeProject match {
        case Some(project) => s"${project.value}: $str"
        case None          => str
      }) {
    def this(str: String) = this(None, str)
    def this(project: model.CrossProjectName, str: String) = this(Some(project), str)
  }

  class Cause(
      val cause: Throwable,
      val error: String
  ) extends BleepException(error, cause)
}
