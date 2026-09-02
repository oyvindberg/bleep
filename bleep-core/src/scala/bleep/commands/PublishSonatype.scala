package bleep
package commands

import bleep.nosbt.InteractionService
import bleep.packaging.*
import bleep.plugin.cirelease.CiReleasePlugin
import bleep.plugin.pgp.PgpPlugin
import bleep.plugin.sonatype.Sonatype

import scala.collection.immutable.SortedMap

object PublishSonatype {
  case class Options(
      version: PublishVersion,
      assertRelease: Boolean,
      projectNames: Array[model.CrossProjectName],
      manifestCreator: ManifestCreator
  )
}

/** Publishes artifacts to Sonatype / Maven Central with GPG signing and staging. */
case class PublishSonatype(options: PublishSonatype.Options, buildOpts: CommonBuildOpts) extends BleepBuildCommand {
  override def run(started: Started): Either[BleepException, Unit] = {
    val projects = Publish.resolveProjects(started.build, options.projectNames)
    if (projects.isEmpty) {
      throw new BleepException.Text("No publishable projects found. Add 'publish' config to projects or specify project names.")
    }

    ReactiveBsp
      .compile(
        watch = false,
        projects = projects,
        displayMode = buildOpts.displayMode,
        flamegraph = buildOpts.flamegraph,
        cancel = buildOpts.cancel,
        diffBase = None,
        diffOutput = OutputMode.Text
      )
      .run(started)
      .map { case () =>
        // One rule, in one place, for every publish command. See PublishVersion.resolve.
        val version = PublishVersion.resolve(options.version, started.buildPaths.buildDir, options.assertRelease).orThrow

        // The release plugin wants the dynver object, not a version string — it reads the git state again to decide whether to release or to publish a
        // snapshot. Built through the same function so `dynverSonatypeSnapshots = true` is stated once rather than in every place that needs a dynver.
        val dynVer = PublishVersion.dynverFor(started.buildPaths.buildDir)
        started.logger.info(s"Publishing version: $version")

        val pgp = new PgpPlugin(
          logger = started.logger,
          maybeCredentials = None,
          interactionService = InteractionService.DoesNotMaskYourPasswordExclamationOneOne
        )

        // Use the first publishable project's config to derive sonatype settings
        val firstPublishConfig = projects.iterator
          .map(n => started.build.explodedProjects(n))
          .flatMap(_.publish)
          .next()

        val groupId = firstPublishConfig.groupId.getOrElse(
          throw new BleepException.Text("No groupId in publish config. Set 'publish.groupId' in bleep.yaml.")
        )
        val profileName = firstPublishConfig.sonatypeProfileName.getOrElse(groupId)
        val credentialHost = firstPublishConfig.sonatypeCredentialHost.getOrElse("central.sonatype.com")

        val sonatype = new Sonatype(
          logger = started.logger,
          sonatypeBundleDirectory = started.buildPaths.dotBleepDir / "sonatype-bundle",
          sonatypeProfileName = profileName,
          bundleName = groupId,
          version = version,
          sonatypeCredentialHost = credentialHost
        )

        val ciRelease = new CiReleasePlugin(started.logger, sonatype, dynVer, pgp)

        val inferredScm = CiReleasePlugin.inferScmInfo
        val info = Publish.toInfo(firstPublishConfig, inferredScm)

        // Package all projects
        val allFiles: Map[RelPath, Array[Byte]] = projects.flatMap { projectName =>
          val project = started.build.explodedProjects(projectName)
          val publishConfig = project.publish.getOrElse(
            throw new BleepException.Text(s"Project ${projectName.value} has no 'publish' config")
          )
          val projGroupId = publishConfig.groupId.getOrElse(groupId)

          val packagedLibraries: SortedMap[model.CrossProjectName, PackagedLibrary] =
            packageLibraries(
              started,
              coordinatesFor = CoordinatesFor.Default(groupId = projGroupId, version = version),
              shouldInclude = Set(projectName),
              publishLayout = PublishLayout.Maven(info),
              manifestCreator = options.manifestCreator
            )

          packagedLibraries.flatMap { case (_, PackagedLibrary(_, files)) => files.all }
        }.toMap

        allFiles.foreach { case (path, bytes) =>
          started.logger.withContext("path", path.asString).withContext("bytes.length", bytes.length).debug("will publish")
        }

        ciRelease.ciRelease(allFiles)
      }
  }
}
