package bleep

import bleep.logging.Logger

import scala.concurrent.ExecutionContext

/** At this point we assert that we *have* a build. it's not necessarily loaded yet
  */
case class Prebootstrapped(
    logger: Logger,
    userPaths: UserPaths,
    buildPaths: BuildPaths,
    existingBuild: BuildLoader.Existing,
    ec: ExecutionContext
) {
  val cacheLogger = new BleepCacheLogger(logger)
  val fetchJvm = new FetchJvm(Some(userPaths.resolveJvmCacheDir), cacheLogger, ec)
  val fetchNode = new FetchNode(cacheLogger, ec)

  private val lastKnownHash = FileHash(existingBuild.bleepYaml)

  def isOutdated(): Boolean = {
    val newHash = FileHash(existingBuild.bleepYaml)
    lastKnownHash != newHash
  }

  val resolvedJvm: Lazy[ResolvedJvm] =
    existingBuild.buildFile.map { maybeBuild =>
      val jvm = maybeBuild.orThrow.jvm.getOrElse {
        logger.warn(
          s"You build uses the default system JVM, which can change outside the build. for stable builds over time, let bleep manage your chosen JVM by adding it to ${BuildLoader.BuildFileName}"
        )
        model.Jvm.system
      }
      fetchJvm(jvm)
    }

  def reloadFromDisk(): Either[BleepException, Prebootstrapped] =
    BuildLoader
      .inDirectory(existingBuild.buildDirectory)
      .existing
      .map(newExisting => copy(existingBuild = newExisting))
}
