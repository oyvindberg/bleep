package bleep

import coursier.cache.{CacheLogger, FileCache}
import coursier.jvm.JvmChannel
import coursier.util.{Artifact, Task}

import java.nio.file.Path
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

object FetchScalafmt {
  def apply(cacheLogger: CacheLogger, ec: ExecutionContext, version: String): Path = {
    val filename = JvmChannel.defaultOs() match {
      case "darwin" =>
        "scalafmt-macos"
      case "linux" =>
        "scalafmt-linux-musl"
      case other =>
        throw new BleepException.Text(s"Sorry, no native scalafmt launcher for $other")
    }

    val fileCache = FileCache[Task]().withLogger(cacheLogger)
    val artifact = Artifact.apply(s"https://github.com/scalameta/scalafmt/releases/download/v$version/$filename")
    val bin = Await.result(fileCache.file(artifact).run.value(ec), Duration.Inf)

    bin match {
      case Left(err) => throw new BleepException.ArtifactResolveError(err, "scalafmt")
      case Right(file) =>
        file.setExecutable(true)
        file.toPath
    }
  }
}
