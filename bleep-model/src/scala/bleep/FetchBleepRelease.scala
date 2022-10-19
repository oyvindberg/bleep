package bleep

import coursier.cache.{ArchiveCache, ArtifactError, CacheLogger, FileCache}
import coursier.jvm.JvmIndex
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

  def apply(wanted: model.BleepVersion, cacheLogger: CacheLogger, executionContext: ExecutionContext): Either[BleepException, Path] =
    apply(wanted, cacheLogger, executionContext, JvmIndex.currentArchitecture, JvmIndex.currentOs)

  def apply(
      wanted: model.BleepVersion,
      cacheLogger: CacheLogger,
      executionContext: ExecutionContext,
      arch: Either[String, String],
      os: Either[String, String]
  ): Either[BleepException, Path] = {
    val base = s"https://github.com/oyvindberg/bleep/releases/download/v${wanted.value}"

    val isOldLayout = oldLayouts(wanted)

    val maybeUrl: Either[String, String] =
      (arch, os) match {
        case (Right("amd64"), Right("darwin")) if isOldLayout  => Right(s"$base/bleep-${wanted.value}-x86-64-apple-darwin.gz")
        case (Right("amd64"), Right("linux")) if isOldLayout   => Right(s"$base/bleep-${wanted.value}-x86-64-pc-linux.gz")
        case (Right("amd64"), Right("windows")) if isOldLayout => Right(s"$base/bleep-${wanted.value}-x86-64-pc-win32.zip")
        case (Right("arm64"), Right("darwin"))                 => Right(s"$base/bleep-arm64-apple-darwin.tar.gz")
        case (Right("amd64"), Right("darwin"))                 => Right(s"$base/bleep-x86_64-apple-darwin.tar.gz")
        case (Right("amd64"), Right("linux"))                  => Right(s"$base/bleep-x86_64-pc-linux.tar.gz")
        case (Right("amd64"), Right("windows"))                => Right(s"$base/bleep-x86_64-pc-win32.zip")
        case (Right(arch), Right(os))                          => Left(s"Unsupported combination of architecture $arch and os $os")
        case (Left(unsupported), _)                            => Left(unsupported)
        case (_, Left(unsupported))                            => Left(unsupported)
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
                JvmIndex.currentOs match {
                  case Right("darwin" | "linux") =>
                    file.setExecutable(true)
                  case _ => ()
                }
                Right(executable.toPath)
            }
        }
    }
  }
}
