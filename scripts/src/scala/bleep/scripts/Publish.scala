package bleep
package scripts

import bleep.nosbt.InteractionService
import bleep.packaging.{packageLibraries, CoordinatesFor, PackagedLibrary, PublishLayout}
import bleep.plugin.cirelease.CiReleasePlugin
import bleep.plugin.dynver.DynVerPlugin
import bleep.plugin.pgp.PgpPlugin
import bleep.plugin.sonatype.Sonatype
import coursier.Info

import scala.collection.immutable.SortedMap

object Publish extends BleepScript("Publish") {
  val groupId = "build.bleep"

  def run(started: Started, commands: Commands, args: List[String]): Unit = {
    commands.compile(started.build.explodedProjects.keys.filter(projectsToPublish.include).toList)

    val dynVer = new DynVerPlugin(baseDirectory = started.buildPaths.buildDir.toFile, dynverSonatypeSnapshots = true)
    val pgp = new PgpPlugin(
      logger = started.logger,
      maybeCredentials = None,
      interactionService = InteractionService.DoesNotMaskYourPasswordExclamationOneOne
    ) {
      // pick one of the keys from `gpg --list-keys`
      override def pgpSigningKey() = Some("0BC13EB20EDBE20BE51010A04F1C4835C3551931")
    }
    val sonatype = new Sonatype(
      logger = started.logger,
      sonatypeBundleDirectory = started.buildPaths.dotBleepDir / "sonatype-bundle",
      sonatypeProfileName = groupId,
      bundleName = "bleep",
      version = dynVer.version,
      sonatypeCredentialHost = Sonatype.sonatype01
    )
    val ciRelease = new CiReleasePlugin(started.logger, sonatype, dynVer, pgp)

    started.logger.info(dynVer.version)

    val info = Info(
      "A bleeping fast scala build tool!",
      "https://github.com/oyvindberg/bleep/",
      List(
        Info.Developer(
          "oyvindberg",
          "Øyvind Raddum Berg",
          "https://github.com/oyvindberg"
        )
      ),
      publication = None,
      scm = CiReleasePlugin.inferScmInfo,
      licenseInfo = List(
        Info.License(
          "MIT",
          Some("http://opensource.org/licenses/MIT"),
          distribution = Some("repo"),
          comments = None
        )
      )
    )

    val packagedLibraries: SortedMap[model.CrossProjectName, PackagedLibrary] =
      packageLibraries(
        started,
        coordinatesFor = CoordinatesFor.Default(groupId = groupId, version = dynVer.version),
        shouldInclude = projectsToPublish.include,
        publishLayout = PublishLayout.Maven(info)
      )

    val files: Map[RelPath, Array[Byte]] =
      packagedLibraries.flatMap { case (_, PackagedLibrary(_, files)) => files.all }

    files.foreach { case (path, bytes) =>
      started.logger.withContext("path", path.asString).withContext("bytes.length", bytes.length).debug("will publish")
    }
    ciRelease.ciRelease(files)
  }
}
