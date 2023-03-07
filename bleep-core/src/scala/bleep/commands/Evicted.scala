package bleep
package commands

import bleep.BleepException
import bleep.depcheck.CheckEvictions
import bleep.model.VersionCombo
import cats.implicits.toTraverseOps
import sbt.librarymanagement.EvictionWarningOptions

case class Evicted(projectNames: Array[model.CrossProjectName]) extends BleepBuildCommand {
  override def run(started: Started): Either[BleepException, Unit] =
    projectNames.toList
      .traverse { projectName =>
        val project = started.build.explodedProjects(projectName)
        val versionCombo = VersionCombo.fromExplodedProject(project).orThrowText
        val bleepDeps = project.dependencies.values

        val res = for {
          coursierDeps <- CoursierResolver.asCoursierDeps(bleepDeps, versionCombo)
          resolved <- started.resolver.direct(bleepDeps, versionCombo, project.libraryVersionSchemes.values)
        } yield CheckEvictions.warnings(
          ewo = EvictionWarningOptions.full,
          versionCombo = versionCombo,
          dependencies = coursierDeps,
          res = resolved,
          logger = started.logger.withPath(s"project ${projectName.value}")
        )
        res.left.map(err => throw new BleepException.ResolveError(err, projectName.value))
      }
      .map(_ => ())
}
