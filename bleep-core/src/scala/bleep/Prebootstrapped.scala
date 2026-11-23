package bleep

import ryddig.Logger

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

  /** The full, standard set of `${TOKEN}`-style replacements for this build+machine (`BUILD_DIR`, `HOME_DIR`, `TEMP_DIR`, the coursier cache dirs) — built
    * once, here, rather than reconstructed narrowly (missing most of the tokens) at whichever inner call site happens to need one.
    */
  val replacements: model.Replacements = model.Replacements.paths(buildPaths.buildDir, userPaths)

  val resolvedJvm: Lazy[ResolvedJvm] =
    existingBuild.buildFile.map {
      case Left(_) =>
        fetchJvm(model.Jvm.system)
      case Right(build) =>
        val jvm = build.jvm.getOrElse {
          logger.warn(
            s"Your build uses the default system JVM, which can change outside the build. For stable builds over time, let bleep manage your chosen JVM by adding it to ${BuildLoader.BuildFileName}"
          )
          model.Jvm.system
        }
        fetchJvm(if (jvm.name.equalsIgnoreCase("managed")) model.Jvm.system else jvm)
    }

  /** Will only reload if there are changes in the json structure, as indicated in the `Option`
    */
  def reloadFromDisk(): Either[BleepException, Option[Prebootstrapped]] =
    for {
      oldJson <- existingBuild.json.forceGet
      reloaded <- existingBuild.reloadFromDisk().existing
      newJson <- reloaded.json.forceGet
    } yield if (oldJson == newJson) None else Some(copy(existingBuild = reloaded))
}
