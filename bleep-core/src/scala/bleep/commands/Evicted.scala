package bleep
package commands

import bleep.depcheck.CheckEvictions
import bleep.nosbt.librarymanagement
import cats.implicits.toTraverseOps

case class Evicted(projectNames: Array[model.CrossProjectName], outputMode: OutputMode) extends BleepBuildCommand {
  override def run(started: Started): Either[BleepException, Unit] =
    projectNames.toList
      .traverse { projectName =>
        val project = started.build.explodedProjects(projectName)
        val versionCombo = model.VersionCombo.fromExplodedProject(project).orThrowText
        val bleepDeps = project.dependencies.values

        val res = for {
          coursierDeps <- CoursierResolver.asCoursierDeps(bleepDeps, versionCombo)
          resolved <- started.resolver.direct(
            bleepDeps,
            versionCombo,
            project.libraryVersionSchemes.values,
            project.ignoreEvictionErrors.getOrElse(model.IgnoreEvictionErrors.No)
          )
        } yield {
          val lines = CheckEvictions.warningLines(
            ewo = librarymanagement.EvictionWarningOptions.full,
            versionCombo = versionCombo,
            dependencies = coursierDeps,
            res = resolved,
            logger = started.logger.withPath(s"project ${projectName.value}")
          )
          outputMode match {
            case OutputMode.Text =>
              lines.foreach(started.logger.withPath(s"project ${projectName.value}").warn(_))
            case OutputMode.Json => ()
            case OutputMode.Raw  =>
              if (lines.nonEmpty) {
                println(projectName.value)
                lines.foreach(l => println(s"  $l"))
              }
          }
          ProjectEvictions(projectName.value, lines)
        }
        res.left.map(err => throw new BleepException.ResolveError(err, projectName.value))
      }
      .map { evictions =>
        outputMode match {
          case OutputMode.Text | OutputMode.Raw => ()
          case OutputMode.Json                  =>
            CommandResult.print(CommandResult.success(EvictionReport(evictions)))
        }
      }
}
