package bleep
package commands

import io.circe.syntax.*
import io.circe.Encoder

/** JSON output commands for IDE integration.
  */
object ExtractInfo {

  case class ProjectInfo(
      name: String,
      dependsOn: List[String],
      isTest: Boolean
  )

  case class GroupInfo(
      name: String,
      projects: List[String]
  )

  case class ProjectGraphOutput(
      projects: List[ProjectInfo]
  )

  case class ProjectGroupsOutput(
      groups: List[GroupInfo]
  )

  case class ScriptInfo(
      name: String,
      project: String,
      mainClass: String
  )

  case class ScriptsOutput(
      scripts: List[ScriptInfo]
  )

  case class SourceGenInfo(
      project: String,
      sourceGenProject: String,
      mainClass: String
  )

  case class SourceGenOutput(
      sourcegens: List[SourceGenInfo]
  )

  implicit val projectInfoEncoder: Encoder[ProjectInfo] =
    Encoder.forProduct3("name", "dependsOn", "isTest")(p => (p.name, p.dependsOn, p.isTest))

  implicit val groupInfoEncoder: Encoder[GroupInfo] =
    Encoder.forProduct2("name", "projects")(g => (g.name, g.projects))

  implicit val projectGraphEncoder: Encoder[ProjectGraphOutput] =
    Encoder.forProduct1("projects")(_.projects)

  implicit val projectGroupsEncoder: Encoder[ProjectGroupsOutput] =
    Encoder.forProduct1("groups")(_.groups)

  implicit val scriptInfoEncoder: Encoder[ScriptInfo] =
    Encoder.forProduct3("name", "project", "mainClass")(s => (s.name, s.project, s.mainClass))

  implicit val scriptsOutputEncoder: Encoder[ScriptsOutput] =
    Encoder.forProduct1("scripts")(_.scripts)

  implicit val sourceGenInfoEncoder: Encoder[SourceGenInfo] =
    Encoder.forProduct3("project", "sourceGenProject", "mainClass")(s => (s.project, s.sourceGenProject, s.mainClass))

  implicit val sourceGenOutputEncoder: Encoder[SourceGenOutput] =
    Encoder.forProduct1("sourcegens")(_.sourcegens)

  /** Outputs project graph as JSON - all projects with their direct dependencies */
  case object ProjectGraph extends BleepBuildCommand {
    override def run(started: Started): Either[BleepException, Unit] = {
      val build = started.build

      val projects = build.explodedProjects.toList.sortBy(_._1.value).map { case (crossName, project) =>
        val deps = build.resolvedDependsOn.getOrElse(crossName, Set.empty).toList.map(_.value).sorted
        ProjectInfo(
          name = crossName.value,
          dependsOn = deps,
          isTest = project.isTestProject.getOrElse(false)
        )
      }

      val output = ProjectGraphOutput(projects = projects)
      println(output.asJson.noSpaces)
      Right(())
    }
  }

  /** Outputs project groups as JSON - convenient groupings for bulk selection */
  case object ProjectGroups extends BleepBuildCommand {
    override def run(started: Started): Either[BleepException, Unit] = {
      val build = started.build
      val globs = started.globs

      val projectNameMap = globs.projectNameMap
      val allProjectNames = build.explodedProjects.keys.map(_.value).toSet

      // Filter to only include groups that map to multiple projects
      // or are cross-ids (like "jvm3") that expand to projects
      val groups = projectNameMap.toList
        .filter { case (groupName, projectArray) =>
          val projectCount = projectArray.length
          // Include if it's a group (maps to multiple projects) or is a cross-id like "jvm3"
          projectCount > 1 || (projectCount == 1 && !allProjectNames.contains(groupName))
        }
        .sortBy { case (name, projects) => (-projects.length, name) }
        .map { case (groupName, projectArray) =>
          GroupInfo(
            name = groupName,
            projects = projectArray.map(_.value).sorted.toList
          )
        }

      val output = ProjectGroupsOutput(groups = groups)
      println(output.asJson.noSpaces)
      Right(())
    }
  }

  /** Outputs scripts as JSON - script name, project, and main class */
  case object Scripts extends BleepBuildCommand {
    override def run(started: Started): Either[BleepException, Unit] = {
      val build = started.build

      val scripts = build.scripts.toList.sortBy(_._1.value).flatMap { case (scriptName, scriptDefs) =>
        scriptDefs.values.collect { case model.ScriptDef.Main(project, main, _) =>
          ScriptInfo(
            name = scriptName.value,
            project = project.value,
            mainClass = main
          )
        }
      }

      val output = ScriptsOutput(scripts = scripts)
      println(output.asJson.noSpaces)
      Right(())
    }
  }

  /** Outputs sourcegen as JSON - projects and their sourcegen definitions */
  case object SourceGen extends BleepBuildCommand {
    override def run(started: Started): Either[BleepException, Unit] = {
      val build = started.build

      val sourcegens = build.explodedProjects.toList.sortBy(_._1.value).flatMap { case (crossName, project) =>
        project.sourcegen.values.toList.collect { case model.ScriptDef.Main(sourceGenProject, main, _) =>
          SourceGenInfo(
            project = crossName.value,
            sourceGenProject = sourceGenProject.value,
            mainClass = main
          )
        }
      }

      val output = SourceGenOutput(sourcegens = sourcegens)
      println(output.asJson.noSpaces)
      Right(())
    }
  }
}
