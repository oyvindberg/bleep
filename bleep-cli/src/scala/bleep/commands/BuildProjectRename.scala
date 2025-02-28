package bleep
package commands

import java.nio.file.Files
import scala.collection.immutable.SortedMap

class BuildProjectRename(from: model.ProjectName, to: model.ProjectName) extends BleepBuildCommand {
  override def run(started: Started): Either[BleepException, Unit] = {
    val fromCrossProjects: List[model.CrossProjectName] =
      started.build.explodedProjects.toList.collect { case (cp @ model.CrossProjectName(`from`, _), _) => cp }

    if (fromCrossProjects.isEmpty)
      return Left(new BleepException.Text(s"Project ${from.value} does not exist"))
    if (started.build.explodedProjects.keys.exists(_.name == to))
      return Left(new BleepException.Text(s"Project ${to.value} already exists"))

    val buildFile = started.build.requireFileBacked(ctx = "command project-rename").file

    def rewriteProject(p: model.Project): model.Project =
      p.copy(
        dependsOn = p.dependsOn.map {
          case `from` => to
          case other  => other
        },
        sourcegen = p.sourcegen.map(rewriteScriptDefs)
      )

    def rewriteScriptDefs(s: model.ScriptDef): model.ScriptDef =
      s match {
        case s @ model.ScriptDef.Main(model.CrossProjectName(`from`, _), _, _) =>
          s.copy(project = model.CrossProjectName(to, s.project.crossId))
        case s => s
      }

    fromCrossProjects.map(started.projectPaths).map(_.dir).distinct match {
      case List(fromDir) =>
        val toDir = fromDir.resolveSibling(to.value)
        if (Files.exists(toDir))
          return Left(new BleepException.Text(s"Expected to move project ${from.value} from $fromDir to $toDir, but it already exists"))

        val newBuildFile = buildFile.copy(
          projects = buildFile.projects.map {
            case (`from`, p) =>
              val folderValue = RelPath.relativeTo(started.buildPaths.buildDir, toDir) match {
                case RelPath(Array(to.value)) => None
                case other                    => Some(other)
              }

              (`to`, p.copy(folder = folderValue))
            case (pn, p) => (pn, rewriteProject(p))
          },
          scripts = buildFile.scripts.map { case (sn, scripts) => (sn, scripts.map(rewriteScriptDefs)) },
          templates = buildFile.templates.map { case (pn, p) => (pn, rewriteProject(p)) }
        )

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
