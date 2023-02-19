package bleep

import bleep.internal.throwableMessages
import bleep.logging.Logger
import coursier.cache.CacheLogger
import coursier.error.CoursierError
import coursier.util.Artifact

class BleepCacheLogger(logger: Logger) extends CacheLogger {
  def retrying(err: CoursierError, retriesLeft: Int): Unit =
    logger.withContext(retriesLeft).info(s"Resolving dependencies failed. Retrying. Error: ${throwableMessages(err).mkString(": ")}")

  override def downloadingArtifact(url: String, artifact: Artifact): Unit =
    logger.withContext(url).info("downloading")

  override def downloadedArtifact(url: String, success: Boolean): Unit =
    if (success) logger.withContext(url).info("downloaded")
    else logger.withContext(url).warn("could not download")
}
