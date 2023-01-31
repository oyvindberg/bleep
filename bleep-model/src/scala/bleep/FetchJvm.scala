package bleep

import bleep.model.Jvm
import coursier.cache.{ArchiveCache, CacheLogger, FileCache}
import coursier.jvm.{JavaHome, JvmCache, JvmIndex}
import coursier.util.Task

import java.nio.file.{Files, Path}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

object FetchJvm {
  def apply(maybeCacheDir: Option[Path], cacheLogger: CacheLogger, jvm: model.Jvm, ec: ExecutionContext): Path = {
    val arch = OsArch.current.arch match {
      case Arch.Amd64 => "amd64"
      case Arch.Arm64 => "arm64"
    }

    maybeCacheDir match {
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
          val fetched = doFetch(cacheLogger, jvm, ec, arch)
          Files.createDirectories(cacheFile.getParent)
          Files.writeString(cacheFile, fetched.toString)
          fetched
        }

      case Some(_) | None => // No cache directory defined, or the system's jvm is used.
        doFetch(cacheLogger, jvm, ec, arch)
    }
  }

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
