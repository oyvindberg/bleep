package bleep

import coursier.cache.{ArchiveCache, ArtifactError, CacheLogger, FileCache}
import coursier.util.{Artifact, Task}

import java.io.File
import java.nio.file.Path
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object FetchBleepRelease {
  // file names were changed in 0.0.1-M8
  val oldLayouts: Set[model.BleepVersion] = Range.inclusive(1, 7).toSet.map((n: Int) => model.BleepVersion(s"0.0.1-M$n"))

  def findExecutable(file: File): Either[String, File] =
    if (file.isDirectory) {
      file.list().toList match {
        case Nil       => Left(s"no files found in directory $file")
        case List(one) => findExecutable(new File(file, one))
        case more      => Left(s"expected one file inside $file, got ${more.mkString(", ")}")
      }
    } else Right(file)

  def apply(
      wanted: model.BleepVersion,
      cacheLogger: CacheLogger,
      executionContext: ExecutionContext,
      osArch: OsArch.HasNativeImage
  ): Either[BleepException, Path] = {
    val base = s"https://github.com/oyvindberg/bleep/releases/download/v${wanted.value}"

    val isOldLayout = oldLayouts(wanted)

    val maybeUrl: Either[String, String] =
      osArch match {
        case OsArch.MacosAmd64 if isOldLayout   => Right(s"$base/bleep-${wanted.value}-x86-64-apple-darwin.gz")
        case OsArch.LinuxAmd64 if isOldLayout   => Right(s"$base/bleep-${wanted.value}-x86-64-pc-linux.gz")
        case OsArch.WindowsAmd64 if isOldLayout => Right(s"$base/bleep-${wanted.value}-x86-64-pc-win32.zip")
        case OsArch.MacosArm64(_)               => Right(s"$base/bleep-arm64-apple-darwin.tar.gz")
        case OsArch.MacosAmd64                  => Right(s"$base/bleep-x86_64-apple-darwin.tar.gz")
        case OsArch.LinuxAmd64                  => Right(s"$base/bleep-x86_64-pc-linux.tar.gz")
        case OsArch.WindowsAmd64                => Right(s"$base/bleep-x86_64-pc-win32.zip")
      }

    maybeUrl match {
      case Left(msg) => Left(new BleepException.Text(msg))
      case Right(uri) =>
        val fetching: Future[Either[ArtifactError, File]] = ArchiveCache[Task]()
          .withCache(FileCache().withLogger(cacheLogger))
          .get(Artifact(uri))
          .value(executionContext)

        Await.result(fetching, Duration.Inf) match {
          case Left(artifactError) =>
            Left(new BleepException.ArtifactResolveError(artifactError, s"bleep version ${wanted.value}"))
          case Right(file) =>
            findExecutable(file) match {
              case Left(msg) =>
                Left(new BleepException.Text(msg))
              case Right(executable) =>
                osArch.os match {
                  case model.Os.Macos | model.Os.Linux =>
                    file.setExecutable(true)
                  case _ => ()
                }
                Right(executable.toPath)
            }
        }
    }
  }
}
