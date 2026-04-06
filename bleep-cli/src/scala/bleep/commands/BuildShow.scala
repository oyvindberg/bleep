package bleep
package commands

import cats.data.NonEmptyList

object BuildShow {
  case class Short(projects: NonEmptyList[model.ProjectName], outputMode: OutputMode) extends BleepBuildCommand {
    override def run(started: Started): Either[BleepException, Unit] = {
      val build = started.build.requireFileBacked("command build show short")
      outputMode match {
        case OutputMode.Text =>
          projects.toList.foreach { projectName =>
            val p = build.file.projects.value(projectName)
            println(fansi.Color.Red(projectName.value))
            println(yaml.encodeShortened(p))
          }
        case OutputMode.Json =>
          val configs = projects.toList.map { projectName =>
            val p = build.file.projects.value(projectName)
            ProjectConfig(projectName.value, yaml.encodeShortened(p))
          }
          CommandResult.print(CommandResult.success(ProjectConfigs(configs)))
      }
      Right(())
    }
  }
  case class Effective(projects: Array[model.CrossProjectName], outputMode: OutputMode) extends BleepBuildCommand {
    override def run(started: Started): Either[BleepException, Unit] = {
      outputMode match {
        case OutputMode.Text =>
          projects.foreach { crossProjectName =>
            val p0 = started.build.explodedProjects(crossProjectName)
            val p = p0.copy(cross = model.JsonMap.empty, `extends` = model.JsonSet.empty)
            println(fansi.Color.Red(crossProjectName.value))
            println(yaml.encodeShortened(p))
          }
        case OutputMode.Json =>
          val configs = projects.toList.map { crossProjectName =>
            val p0 = started.build.explodedProjects(crossProjectName)
            val p = p0.copy(cross = model.JsonMap.empty, `extends` = model.JsonSet.empty)
            ProjectConfig(crossProjectName.value, yaml.encodeShortened(p))
          }
          CommandResult.print(CommandResult.success(ProjectConfigs(configs)))
      }
      Right(())
    }
  }
}
