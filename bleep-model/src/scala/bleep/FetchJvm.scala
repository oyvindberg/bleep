package bleep

import coursier.cache.{ArchiveCache, CacheLogger, FileCache}
import coursier.jvm.{JavaHome, JvmCache, JvmIndex}
import coursier.util.Task

import java.nio.file.Path
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

object FetchJvm {
  def apply(cacheLogger: CacheLogger, jvm: model.Jvm, ec: ExecutionContext): Path = {
    val architecture = JvmIndex.defaultOs() match {
      case "darwin" =>
        import scala.sys.process._
        val uname = Seq("uname", "-a").!!.toLowerCase
        if (uname.contains("arm64")) "arm64" else JvmIndex.defaultArchitecture()
      case _ => JvmIndex.defaultArchitecture()
    }
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
