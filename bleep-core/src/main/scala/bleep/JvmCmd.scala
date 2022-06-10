package bleep

import bleep.logging.Logger
import coursier.jvm.{JavaHome, JvmCache, JvmIndex}

import java.nio.file.Path
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

object JvmCmd {
  def apply(logger: Logger, jvm0: Option[model.Jvm], ec: ExecutionContext): Path = {
    val jvm = jvm0.getOrElse(model.Jvm.System)
    val javaBin = Await.result(
      JavaHome()
        .withCache(
          JvmCache().withIndex(jvm.index.getOrElse(JvmIndex.coursierIndexUrl))
        )
        .javaBin(jvm.name)
        .value(ec),
      Duration.Inf
    )
    logger.withContext(javaBin).debug(s"Resolved JVM ${jvm0.map(_.name)}")
    javaBin
  }
}
