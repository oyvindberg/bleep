package bleep

import bleep.internal.CoursierLogger
import bleep.logging.Logger
import coursier.cache.{ArchiveCache, FileCache}
import coursier.jvm.{JavaHome, JvmCache, JvmIndex}
import coursier.util.Task

import java.nio.file.Path
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

object FetchJvm {
  def apply(logger: Logger, jvm0: Option[model.Jvm], ec: ExecutionContext): Path = {
    val jvm = jvm0.getOrElse(model.Jvm.System)
    val architecture = JvmIndex.defaultOs() match {
      case "darwin" =>
        import scala.sys.process._
        val uname = Seq("uname", "-a").!!.toLowerCase
        if (uname.contains("arm64")) "arm64" else JvmIndex.defaultArchitecture()
      case _ => JvmIndex.defaultArchitecture()
    }
    val fileCache = FileCache[Task]().withLogger(new CoursierLogger(logger))
    val archiveCache = ArchiveCache[Task]().withCache(fileCache)
    val jvmCache = JvmCache()
      .withArchiveCache(archiveCache)
      .withIndex(jvm.index.getOrElse(JvmIndex.coursierIndexUrl))
      .withArchitecture(architecture)
    val javaBin = Await.result(JavaHome().withCache(jvmCache).javaBin(jvm.name).value(ec), Duration.Inf)
    logger.withContext(javaBin).debug(s"Resolved JVM ${jvm0.map(_.name).getOrElse("")}")
    javaBin
  }
}
