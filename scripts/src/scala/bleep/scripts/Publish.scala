package bleep
package scripts

import bleep.RelPath
import bleep.tasks.publishing._
import coursier.Info
import nosbt.InteractionService

import scala.collection.immutable.SortedMap

object Publish extends BleepScriptRunner("Publish") {
  val groupId = "build.bleep"

  def run(started: Started, commands: Commands, args: List[String]): Unit = {
    commands.compile(started.build.projects.keys.filter(projectsToPublish.include).toList)

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

    val info = Info(
      "A bleeping fast scala build tool!",
      "https://github.com/oyvindberg/bleep/",
      List(
        Info.Developer(
          "oyvindberg",
          "Øyvind Raddum Berg",
          "https://github.com/oyvindberg"
        ),
        Info.Developer(
          "hamnis",
          "Erlend Hamnaberg",
          "https://github.com/hamnis"
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

    val bundledProjects: SortedMap[model.CrossProjectName, Deployable] =
      fileBundle(
        started,
        asDep = (crossName, _) => model.Dep.Scala(org = groupId, name = crossName.name.value, version = dynVer.version),
        shouldInclude = projectsToPublish.include,
        bundleLayout = fileBundle.BundleLayout.Maven(info)
      )

    val bundleFiles: Map[RelPath, Array[Byte]] =
      bundledProjects.flatMap { case (_, Deployable(_, files)) => files.all }

    ciRelease.ciRelease(bundleFiles)
  }
}
