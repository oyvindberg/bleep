package bleep.internal

import bleep.logging.Logger
import bleep.{model, BuildException}
import coursier.cache.{ArchiveCache, FileCache}
import coursier.jvm.JvmIndex
import coursier.util.{Artifact, Task}

import java.nio.file.Path
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

object FetchBleepRelease {
  def apply(wanted: model.Version, logger: Logger, executionContext: ExecutionContext): Either[BuildException, Path] = {
    val base = s"https://github.com/oyvindberg/bleep/releases/download/v${wanted.value}"

    val maybeUrl: Either[String, String] =
      (JvmIndex.currentArchitecture, JvmIndex.currentOs) match {
        case (Right("amd64"), Right("darwin"))  => Right(s"$base/bleep-${wanted.value}-x86-64-apple-darwin.gz")
        case (Right("amd64"), Right("linux"))   => Right(s"$base/bleep-${wanted.value}-x86-64-pc-linux.gz")
        case (Right("amd64"), Right("windows")) => Right(s"$base/bleep-${wanted.value}-x86-64-pc-win32.zip")
        case (Right(arch), Right(os))           => Left(s"Unsupported combination of architecture $arch and os $os")
        case (Left(unsupported), _)             => Left(unsupported)
        case (_, Left(unsupported))             => Left(unsupported)
      }

    maybeUrl match {
      case Left(msg) => Left(new BuildException.Text(msg))
      case Right(uri) =>
        Await.result(
          ArchiveCache[Task]()
            .withCache(FileCache().withLogger(new CoursierLogger(logger)))
            .get(Artifact(uri))
            .value(executionContext),
          Duration.Inf
        ) match {
          case Left(artifactError) =>
            Left(new BuildException.ArtifactResolveError(artifactError, s"bleep version ${wanted.value}"))
          case Right(file) =>
            JvmIndex.currentOs match {
              case Right("darwin" | "linux") =>
                file.setExecutable(true)
              case _ => ()
            }
            Right(file.toPath)
        }
    }
  }
}
