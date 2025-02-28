package bleep

import coursier.cache.CacheLogger
import coursier.error.CoursierError
import coursier.util.Artifact
import ryddig.Logger
import ryddig.Throwables.messagesFrom

class BleepCacheLogger(logger: Logger) extends CacheLogger {
  def retrying(err: CoursierError, retriesLeft: Int): Unit =
    logger.withContext("retriesLeft", retriesLeft).info(s"Resolving dependencies failed. Retrying. Error: ${messagesFrom(err).mkString(": ")}")

  override def downloadingArtifact(url: String, artifact: Artifact): Unit =
    logger.withContext("url", url).info("downloading")

  override def downloadedArtifact(url: String, success: Boolean): Unit =
    if (success) logger.withContext("url", url).info("downloaded")
    else logger.withContext("url", url).warn("could not download")
}
