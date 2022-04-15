package bleep
package scripts

import bleep.tasks.publishing._
import nosbt.InteractionService

import scala.collection.immutable.SortedMap

object Publish extends App {
  // will publish these with dependencies
  val Wanted = Set(
    model.ProjectName("bleep-tasks"),
    model.ProjectName("bleep-tasks-publishing")
  )

  val groupId = "build.bleep.test"

  bootstrap.forScript("Publish") { started =>
    val dynVer = new DynVerPlugin(baseDirectory = started.buildPaths.buildDir.toFile, dynverSonatypeSnapshots = true)
    val pgp = new PgpPlugin(
      logger = started.logger,
      maybeCredentials = None,
      interactionService = InteractionService.DoesNotMaskYourPasswordExclamationOneOne
    )
    val sonatype = new Sonatype(
      logger = started.logger,
      sonatypeBundleDirectory = started.buildPaths.dotBleepDir / "sonatype-bundle",
      sonatypeProfileName = "build.bleep",
      bundleName = "bleep",
      version = dynVer.version,
      sonatypeCredentialHost = Sonatype.sonatype01
    )
    val ciRelease = new CiReleasePlugin(started.logger, sonatype, dynVer, pgp)

    started.logger.info(dynVer.version)

    val bundledProjects: SortedMap[model.CrossProjectName, Deployable] =
      fileBundle(
        started,
        asDep = (crossName, _) => Dep.Scala(groupId, dynVer.version, crossName.name.value),
        shouldInclude = crossName => Wanted(crossName.name)
      )

    val bundleFiles: Map[RelPath, Array[Byte]] =
      bundledProjects.flatMap { case (_, Deployable(_, files)) => files.all }

    ciRelease.ciRelease(bundleFiles)
  }
}
