package bleep
package commands

import java.nio.file.{Files, Path}
import scala.collection.immutable.SortedMap

/** Remote build cache: pull pre-compiled classes from S3, push after building.
  *
  * Cache entries are per-project tar.gz archives keyed by content digest. The digest captures project config, source content, resource content, and transitive
  * dependency digests.
  *
  * Resources affect the digest but are NOT included in the archive (they're already on disk and don't need compilation).
  *
  * Zinc analysis IS included so incremental compilation works correctly after pulling.
  */
object RemoteCache {

  case class Pull(projects: Array[model.CrossProjectName]) extends BleepBuildCommand {
    override def run(started: Started): Either[BleepException, Unit] = {
      val cacheConfig = started.build match {
        case fb: model.Build.FileBacked => fb.file.`remote-cache`
        case _                          => None
      }
      cacheConfig match {
        case None =>
          Left(new BleepException.Text("No remote-cache configured in bleep.yaml. Add:\n  remote-cache:\n    uri: s3://bucket/prefix\n    region: us-east-1"))
        case Some(config) =>
          val credentials = resolveCredentials(started)
          val client = S3Client.fromConfig(started.logger, config, credentials)
          val prefix = S3Client.keyPrefix(config)

          val digests = ProjectDigest.computeAll(started.build, started.buildPaths)
          val projectsToPull = if (projects.nonEmpty) projects.toSet else digests.keySet

          var pulled = 0
          var skipped = 0
          var notFound = 0

          projectsToPull.toList.sorted.foreach { crossName =>
            digests.get(crossName) match {
              case None =>
                started.logger.warn(s"Project ${crossName.value} not in build, skipping")
              case Some(digest) =>
                val key = cacheKey(prefix, crossName, digest)
                val projectPaths = started.buildPaths.project(crossName, started.build.explodedProjects(crossName))

                if (Files.isDirectory(projectPaths.classes) && Files.list(projectPaths.classes).findAny().isPresent) {
                  skipped += 1
                  started.logger.debug(s"${crossName.value}: already compiled, skipping")
                } else if (client.headObject(key)) {
                  val archive = client.getObject(key)
                  TarGz.unpack(archive, projectPaths.targetDir)
                  pulled += 1
                  started.logger.info(s"${crossName.value}: pulled from cache (${archive.length / 1024}KB)")
                } else {
                  notFound += 1
                  started.logger.debug(s"${crossName.value}: not in cache")
                }
            }
          }

          started.logger.info(s"Remote cache pull: $pulled pulled, $skipped already compiled, $notFound not cached (${projectsToPull.size} total)")
          Right(())
      }
    }
  }

  case class Push(projects: Array[model.CrossProjectName]) extends BleepBuildCommand {
    override def run(started: Started): Either[BleepException, Unit] = {
      val cacheConfig = started.build match {
        case fb: model.Build.FileBacked => fb.file.`remote-cache`
        case _                          => None
      }
      cacheConfig match {
        case None =>
          Left(new BleepException.Text("No remote-cache configured in bleep.yaml"))
        case Some(config) =>
          val credentials = resolveCredentials(started)
          val client = S3Client.fromConfig(started.logger, config, credentials)
          val prefix = S3Client.keyPrefix(config)

          val digests = ProjectDigest.computeAll(started.build, started.buildPaths)
          val projectsToPush = if (projects.nonEmpty) projects.toSet else digests.keySet

          var pushed = 0
          var skipped = 0
          var notCompiled = 0

          projectsToPush.toList.sorted.foreach { crossName =>
            digests.get(crossName) match {
              case None =>
                started.logger.warn(s"Project ${crossName.value} not in build, skipping")
              case Some(digest) =>
                val key = cacheKey(prefix, crossName, digest)
                val projectPaths = started.buildPaths.project(crossName, started.build.explodedProjects(crossName))

                if (!Files.isDirectory(projectPaths.classes) || !Files.list(projectPaths.classes).findAny().isPresent) {
                  notCompiled += 1
                  started.logger.debug(s"${crossName.value}: not compiled, skipping")
                } else if (client.headObject(key)) {
                  skipped += 1
                  started.logger.debug(s"${crossName.value}: already in cache")
                } else {
                  // Pack classes dir + zinc analysis
                  val archive = TarGz.pack(projectPaths.targetDir)
                  client.putObject(key, archive)
                  pushed += 1
                  started.logger.info(s"${crossName.value}: pushed to cache (${archive.length / 1024}KB)")
                }
            }
          }

          started.logger.info(s"Remote cache push: $pushed pushed, $skipped already cached, $notCompiled not compiled (${projectsToPush.size} total)")
          Right(())
      }
    }
  }

  private def cacheKey(prefix: String, crossName: model.CrossProjectName, digest: String): String = {
    val projectKey = crossName.value.replace('/', '-')
    if (prefix.isEmpty) s"$projectKey/$digest.tar.gz"
    else s"$prefix/$projectKey/$digest.tar.gz"
  }

  private def resolveCredentials(started: Started): model.RemoteCacheCredentials =
    started.config.remoteCacheCredentials
      .orElse(model.RemoteCacheCredentials.fromEnv())
      .getOrElse(
        throw new BleepException.Text(
          "No remote cache credentials found. Set remoteCacheCredentials in ~/.config/bleep/config.yaml or BLEEP_REMOTE_CACHE_S3_ACCESS_KEY_ID/BLEEP_REMOTE_CACHE_S3_SECRET_ACCESS_KEY env vars."
        )
      )
}
