package bleep

import coursier.cache.CacheLogger
import coursier.util.Artifact

import java.nio.file.Path
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

object FetchKtfmt {
  val DefaultVersion = "0.62"

  def apply(cacheLogger: CacheLogger, ec: ExecutionContext, version: String): Path = {
    val fileCache = BleepFileCache().withLogger(cacheLogger)
    val artifact = Artifact.apply(
      s"https://repo1.maven.org/maven2/com/facebook/ktfmt/$version/ktfmt-$version-with-dependencies.jar"
    )
    val jar = Await.result(fileCache.file(artifact).run.value(ec), Duration.Inf)

    jar match {
      case Left(err)   => throw new BleepException.ArtifactResolveError(err, "ktfmt")
      case Right(file) => file.toPath
    }
  }
}
