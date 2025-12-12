package bleep

import coursier.cache.{CacheLogger, FileCache}
import coursier.util.{Artifact, Task}

import java.nio.file.Path
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

object FetchGoogleJavaFormat {
  val DefaultVersion = "1.24.0"

  def apply(cacheLogger: CacheLogger, ec: ExecutionContext, version: String): Path = {
    val fileCache = FileCache[Task]().withLogger(cacheLogger)
    val artifact = Artifact.apply(
      s"https://github.com/google/google-java-format/releases/download/v$version/google-java-format-$version-all-deps.jar"
    )
    val jar = Await.result(fileCache.file(artifact).run.value(ec), Duration.Inf)

    jar match {
      case Left(err)   => throw new BleepException.ArtifactResolveError(err, "google-java-format")
      case Right(file) => file.toPath
    }
  }
}
