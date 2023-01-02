package bleep

import coursier.cache.{ArchiveCache, CacheLogger, FileCache}
import coursier.util.{Artifact, Task}

import java.nio.file.Path
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

class FetchNode(logger: CacheLogger, ec: ExecutionContext) {
  def apply(nodeVersion: String): Path = {
    val url = OsArch.current match {
      case OsArch.MacosArm64(_) => s"https://nodejs.org/dist/v$nodeVersion/node-v$nodeVersion-darwin-arm64.tar.gz"
      case OsArch.MacosAmd64    => s"https://nodejs.org/dist/v$nodeVersion/node-v$nodeVersion-darwin-x64.tar.gz"
      case OsArch.WindowsAmd64  => s"https://nodejs.org/dist/v$nodeVersion/node-v$nodeVersion-win-x64.zip"
      case OsArch.LinuxAmd64    => s"https://nodejs.org/dist/v$nodeVersion/node-v$nodeVersion-linux-x64.tar.gz"
      case other                => throw new BleepException.Text(s"todo: implement FetchNode for $other")
    }
    val fileCache = FileCache[Task]().withLogger(logger)
    val cache = ArchiveCache[Task]().withCache(fileCache)

    Await.result(cache.get(Artifact(url)).value(ec), Duration.Inf) match {
      case Left(value) => throw new BleepException.Cause(value, s"couldn't download node $nodeVersion from url $url")
      case Right(folder) =>
        val binaryName = if (OsArch.current.os == model.Os.Windows) "node.exe" else "bin/node"
        val nodeBin = folder.toPath / folder.getName.replace(".tar.gz", "").replace(".zip", "") / binaryName
        if (!nodeBin.toFile.exists()) {
          sys.error(s"Expected $nodeBin to exist")
        }
        nodeBin
    }
  }
}
