package bleep.internal

import bleep.logging.Logger
import coursier.cache.CacheLogger
import coursier.util.Artifact

class CoursierLogger(logger: Logger) extends CacheLogger {
  override def downloadingArtifact(url: String, artifact: Artifact): Unit =
    logger.withContext(url).info("downloading")

  override def downloadedArtifact(url: String, success: Boolean): Unit =
    if (success) logger.withContext(url).info("downloaded")
    else logger.withContext(url).warn("could not download")
}
