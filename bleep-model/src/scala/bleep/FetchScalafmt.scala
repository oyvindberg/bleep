package bleep

import coursier.cache.{ArchiveCache, CacheLogger, FileCache}
import coursier.util.{Artifact, Task}

import java.nio.file.Path
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

object FetchScalafmt {
  private case class ScalafmtAsset(filename: String, isArchive: Boolean)

  def apply(cacheLogger: CacheLogger, ec: ExecutionContext, version: String): Path = {
    val asset = OsArch.current match {
      case OsArch.MacosAmd64    => ScalafmtAsset("scalafmt-macos", isArchive = false)
      case OsArch.MacosArm64(_) => ScalafmtAsset("scalafmt-aarch64-apple-darwin.zip", isArchive = true)
      case OsArch.LinuxAmd64    => ScalafmtAsset("scalafmt-linux-musl", isArchive = false)
      case OsArch.LinuxArm64    => ScalafmtAsset("scalafmt-aarch64-pc-linux.zip", isArchive = true)
      case other                => throw new BleepException.Text(s"Sorry, no native scalafmt launcher for $other")
    }

    val url = s"https://github.com/scalameta/scalafmt/releases/download/v$version/${asset.filename}"
    val fileCache = BleepFileCache().withLogger(cacheLogger)

    if (asset.isArchive) fetchArchive(fileCache, ec, url)
    else fetchStandalone(fileCache, ec, url)
  }

  private def fetchStandalone(fileCache: FileCache[Task], ec: ExecutionContext, url: String): Path = {
    val bin = Await.result(fileCache.file(Artifact(url)).run.value(ec), Duration.Inf)
    bin match {
      case Left(err)   => throw new BleepException.ArtifactResolveError(err, "scalafmt")
      case Right(file) =>
        file.setExecutable(true)
        file.toPath
    }
  }

  private def fetchArchive(fileCache: FileCache[Task], ec: ExecutionContext, url: String): Path = {
    val cache = ArchiveCache[Task]().withCache(fileCache)
    val bin = Await.result(cache.get(Artifact(url)).value(ec), Duration.Inf)
    bin match {
      case Left(err)  => throw new BleepException.ArtifactResolveError(err, "scalafmt")
      case Right(dir) =>
        FetchBleepRelease.findExecutable(dir) match {
          case Left(msg)   => throw new BleepException.Text(s"Could not find scalafmt executable: $msg")
          case Right(file) =>
            file.setExecutable(true)
            file.toPath
        }
    }
  }
}
