package bleep

import bleep.model.Jvm
import coursier.cache.{ArchiveCache, CacheLogger, FileCache}
import coursier.jvm.{JavaHome, JvmCache, JvmIndex}
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
      case Some(cacheDir) if !Jvm.isSystem(jvm) =>
        val cacheFile = {
          val relPath = RelPath(
            List(Some(arch), jvm.index, Some(jvm.name)).flatten
              // somewhat windows safe
              .map(_.replace(":", "_"))
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
  def doFetch(cacheLogger: CacheLogger, jvm: Jvm, ec: ExecutionContext, arch: String): Path = {
    val fileCache = FileCache[Task]().withLogger(cacheLogger)
    val archiveCache = ArchiveCache[Task]().withCache(fileCache)
    val jvmCache = JvmCache()
      .withArchiveCache(archiveCache)
      .withIndex(jvm.index.getOrElse(JvmIndex.coursierIndexUrl))
      .withArchitecture(arch)
    val javaBin = Await.result(JavaHome().withCache(jvmCache).javaBin(jvm.name).value(ec), Duration.Inf)
    javaBin
  }
}
