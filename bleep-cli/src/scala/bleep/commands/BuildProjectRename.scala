package bleep
package commands

import java.nio.file.Files
import scala.collection.immutable.SortedMap

class BuildProjectRename(from: model.ProjectName, to: model.ProjectName) extends BleepBuildCommand {
  override def run(started: Started): Either[BleepException, Unit] = {
    val fromCrossProjects: List[model.CrossProjectName] =
      started.build.explodedProjects.toList.collect { case (cp @ model.CrossProjectName(`from`, _), _) => cp }

    if (fromCrossProjects.isEmpty)
      return Left(new BleepException.Text(s"Project $from does not exist"))
    if (started.build.explodedProjects.keys.exists(_.name == to))
      return Left(new BleepException.Text(s"Project $to already exists"))

    val buildFile = started.build.requireFileBacked(ctx = "command project-rename").file

    def rewriteProject(p: model.Project): model.Project =
      p.copy(
        dependsOn = p.dependsOn.map {
          case `from` => to
          case other  => other
        }
      )

    def rewriteScriptDefs(sd: model.JsonList[model.ScriptDef]): model.JsonList[model.ScriptDef] =
      sd.map {
        case s @ model.ScriptDef.Main(model.CrossProjectName(`from`, _), _, _) =>
          s.copy(project = model.CrossProjectName(to, s.project.crossId))
        case s => s
      }

    val newBuildFile = buildFile.copy(
      projects = buildFile.projects.map {
        case (`from`, p) => (`to`, rewriteProject(p))
        case (pn, p)     => (pn, rewriteProject(p))
      },
      scripts = buildFile.scripts.map { case (sn, sd) => (sn, rewriteScriptDefs(sd)) },
      templates = buildFile.templates.map { case (pn, p) => (pn, rewriteProject(p)) }
    )

    fromCrossProjects.map(started.projectPaths).map(_.dir).distinct match {
      case List(fromDir) =>
        val toDir = fromDir.resolveSibling(to.value)
        if (Files.exists(toDir))
          return Left(new BleepException.Text(s"Expected to move project ${from.value} from $fromDir to $toDir, but it already exists"))

        Right(
          commit(
            started.logger,
            started.buildPaths,
            filesToMove = SortedMap(fromDir -> toDir),
            rewrittenBuild = newBuildFile
          )
        )

      case more => Left(new BleepException.Text(s"Multiple directories for project $from: $more"))
    }
  }
}
