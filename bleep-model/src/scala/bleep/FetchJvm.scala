package bleep

import coursier.cache.{ArchiveCache, CacheLogger, FileCache}
import coursier.jvm.{JavaHome, JvmCache, JvmChannel}
import coursier.util.Task

import java.nio.file.{Files, Path}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

case class ResolvedJvm(jvm: model.Jvm, javaBin: Path) {
  override def toString: String = super.toString
}

case class FetchJvm(maybeCacheDir: Option[Path], cacheLogger: CacheLogger, ec: ExecutionContext) {
  def apply(jvm: model.Jvm): ResolvedJvm = {
    val arch = OsArch.current.arch match {
      case Arch.Amd64 => "amd64"
      case Arch.Arm64 => "arm64"
    }

    val javaBin = maybeCacheDir match {
      case Some(cacheDir) if !model.Jvm.isSystem(jvm) =>
        val cacheFile = {
          val relPath = RelPath.of(
            // somewhat windows safe
            List(Some(arch), jvm.index, Some(jvm.name)).flatten.map(_.replace(":", "_"))*
          )

          cacheDir / relPath
        }
        val found = if (Files.exists(cacheFile)) {
          val shouldBeAtPath = Path.of(Files.readString(cacheFile))
          if (Files.exists(shouldBeAtPath)) Some(shouldBeAtPath) else None
        } else None

        found.getOrElse {
          val fetched = FetchJvm.doFetch(cacheLogger, jvm, ec, arch)
          Files.createDirectories(cacheFile.getParent)
          Files.writeString(cacheFile, fetched.toString)
          fetched
        }

      case Some(_) | None => // No cache directory defined, or the system's jvm is used.
        FetchJvm.doFetch(cacheLogger, jvm, ec, arch)
    }
    ResolvedJvm(jvm, javaBin)

  }
}

object FetchJvm {
  def doFetch(cacheLogger: CacheLogger, jvm: model.Jvm, ec: ExecutionContext, arch: String): Path = {
    val fileCache = FileCache[Task]().withLogger(cacheLogger)
    val jvmCache = JvmCache()
      .withArchiveCache(ArchiveCache[Task]().withCache(fileCache))
      .withIndex(jvm.index.getOrElse(JvmChannel.gitHubIndexUrl))
      .withArchitecture(arch)
    val javaBin = Await.result(JavaHome().withCache(jvmCache).javaBin(jvm.name).value(ec), Duration.Inf)
    javaBin
  }
}
