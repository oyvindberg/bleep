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
    val architecture = JvmIndex.defaultOs() match {
      case "darwin" =>
        // break out of amd64 jail on arm64
        import scala.sys.process._
        val uname = Seq("uname", "-a").!!.toLowerCase
        if (uname.contains("arm64")) "arm64" else JvmIndex.defaultArchitecture()
      case _ => JvmIndex.defaultArchitecture()
    }

    maybeCacheDir match {
      case Some(cacheDir) =>
        val cacheFile = {
          val relPath = RelPath(
            List(Some(architecture), jvm.index, Some(jvm.name)).flatten
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
          val fetched = doFetch(cacheLogger, jvm, ec, architecture)
          Files.createDirectories(cacheFile.getParent)
          Files.writeString(cacheFile, fetched.toString)
          fetched
        }

      case None => doFetch(cacheLogger, jvm, ec, architecture)
    }
  }

  def doFetch(cacheLogger: CacheLogger, jvm: Jvm, ec: ExecutionContext, architecture: String): Path = {
    val fileCache = FileCache[Task]().withLogger(cacheLogger)
    val archiveCache = ArchiveCache[Task]().withCache(fileCache)
    val jvmCache = JvmCache()
      .withArchiveCache(archiveCache)
      .withIndex(jvm.index.getOrElse(JvmIndex.coursierIndexUrl))
      .withArchitecture(architecture)
    val javaBin = Await.result(JavaHome().withCache(jvmCache).javaBin(jvm.name).value(ec), Duration.Inf)
    javaBin
  }
}
