package bleep

import coursier.cache.{ArchiveCache, CacheLogger, FileCache}
import coursier.jvm.JvmChannel
import coursier.util.{Artifact, Task}

import java.nio.file.Path
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

class FetchSbt(logger: CacheLogger, ec: ExecutionContext) {
  def apply(version: String): Path = {
    val url = s"https://github.com/sbt/sbt/releases/download/v$version/sbt-$version.zip"
    val fileCache = FileCache[Task]().withLogger(logger)
    val cache = ArchiveCache[Task]().withCache(fileCache)
    val os = JvmChannel.defaultOs()

    Await.result(cache.get(Artifact(url)).value(ec), Duration.Inf) match {
      case Left(value) => throw new BleepException.Cause(value, s"couldn't download sbt version $version from url $url")
      case Right(folder) =>
        val bin = folder.toPath / "sbt/bin" / (if (os == "windows") "sbt.bat" else "sbt")
        if (!bin.toFile.exists()) {
          sys.error(s"Expected $bin to exist")
        }
        bin
    }
  }
}
