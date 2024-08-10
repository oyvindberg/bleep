package bleep

import coursier.cache.ArtifactError
import coursier.error.CoursierError

import java.nio.file.Path

/** The pattern for error handling in bleep:
  *   - every function where 1) a failure may be handled upstream, or 2) it's a point that it's pure, must return a coproduct like `Either`
  *   - everything else fails by throwing `BleepException`s.
  *
  * Other exceptions should be wrapped in `BleepException` with a suitable text, but a lot of work remains in that direction
  */
abstract class BleepException(
    val message: String,
    cause: Throwable = null
) extends Exception(message, cause)
    with Serializable

object BleepException {
  class BuildNotFound(cwd: Path) extends BleepException(s"Couldn't find ${BuildLoader.BuildFileName} in directories in or above $cwd")

  class TargetFolderNotDetermined(projectName: model.CrossProjectName)
      extends BleepException(s"Couldn't determine original output directory of project ${projectName.name}")

  class InvalidJson(file: Path, e: Throwable) extends BleepException(s"Couldn't parse json/yaml file $file", e)

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

  class Cause[Th <: Throwable: Not](
      val cause: Th,
      val error: String
  ) extends BleepException(error, cause) {
    implicitly[Not[Th]].discard()
  }

  sealed trait Not[T]

  object Not extends NotLower {
    // clash
    implicit def not1[BE <: BleepException]: Not[BE] = null
    implicit def not2[BE <: BleepException]: Not[BE] = null
  }

  trait NotLower {
    implicit def not[T <: Throwable]: Not[T] = null
  }

  /** Some parts of the codebase neatly use `Either`, but for the main parts the pattern is to throw `BleepException`. These are caught in `main`.
    */
  final class ExpectOps[L, R](private val e: Either[L, R]) extends AnyVal {
    def orThrow(implicit ev: L <:< BleepException): R = e match {
      case Left(th)     => throw ev(th)
      case Right(value) => value
    }

    def orThrowWithError[Th <: Throwable with L](error: String)(implicit ev0: L =:= Th, ev: BleepException.Not[Th]): R = e match {
      case Left(th)     => throw new BleepException.Cause[Th](th, error)
      case Right(value) => value
    }

    def orThrowText(implicit ev0: L =:= String): R = e match {
      case Left(msg)    => throw new BleepException.Text(None, msg)
      case Right(value) => value
    }

    def orThrowTextWithContext(context: model.CrossProjectName)(implicit ev0: L =:= String): R = e match {
      case Left(msg)    => throw new BleepException.Text(Some(context), msg)
      case Right(value) => value
    }
  }
}
