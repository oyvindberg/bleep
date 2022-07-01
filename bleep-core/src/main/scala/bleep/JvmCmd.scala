package bleep

import bleep.logging.Logger
import coursier.jvm.{JavaHome, JvmCache, JvmIndex}

import java.nio.file.Path
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

object JvmCmd {
  def apply(logger: Logger, jvm0: Option[model.Jvm], ec: ExecutionContext): Path = {
    val jvm = jvm0.getOrElse(model.Jvm.System)
    val architecture = JvmIndex.defaultOs() match {
      case "darwin" =>
        import scala.sys.process._
        val uname = Seq("uname", "-a").!!.toLowerCase
        if (uname.contains("arm64")) "arm64" else JvmIndex.defaultArchitecture()
      case _ => JvmIndex.defaultArchitecture()
    }

    val javaBin = Await.result(
      JavaHome()
        .withCache(
          JvmCache().withIndex(jvm.index.getOrElse(JvmIndex.coursierIndexUrl)).withArchitecture(architecture)
        )
        .javaBin(jvm.name)
        .value(ec),
      Duration.Inf
    )
    logger.withContext(javaBin).debug(s"Resolved JVM ${jvm0.map(_.name)}")
    javaBin
  }
}
