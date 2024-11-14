package bleep
package commands

import bleep.rewrites.BuildRewrite

import java.nio.file.{Files, Path}
import scala.collection.immutable.SortedMap

class BuildProjectMergeInto(projectName: model.ProjectName, into: model.ProjectName) extends BleepBuildCommand {
  object rewrite extends BuildRewrite {
    override val name: model.BuildRewriteName = model.BuildRewriteName("merge-into")

    def rewriteProject(p: model.Project): model.Project =
      p.copy(
        dependsOn = p.dependsOn.map {
          case `projectName` => into
          case other         => other
        },
        sourcegen = p.sourcegen.map(rewriteScriptDef)
      )

    override protected def newExplodedProjects(oldBuild: model.Build, buildPaths: BuildPaths): Map[model.CrossProjectName, model.Project] =
      oldBuild.explodedProjects.flatMap {
        case (crossName, _) if crossName.name == projectName =>
          None
        case (crossName, p0) if crossName.name == into =>
          val p1 = {
            // copy everything from `projectName` (with non-mergeables from `into` taking precedence)
            val copyFrom = oldBuild.explodedProjects(crossName.copy(name = projectName))
            p0.union(copyFrom)
          }
          // remove `dependsOn` from `projectName` and `into` or transitive dependencies of `into`
          val p2 = {
            val removeDependsOn = oldBuild.transitiveDependenciesFor(crossName).keySet ++ Iterable(projectName, into)
            p1.copy(dependsOn = p1.dependsOn.filterNot(removeDependsOn))
          }
          Some(crossName -> p2)
        case (crossName, p) => Some((crossName, rewriteProject(p)))
      }
  }

  def rewriteScriptDef(s: model.ScriptDef): model.ScriptDef =
    s match {
      case s @ model.ScriptDef.Main(model.CrossProjectName(`projectName`, _), _, _) =>
        s.copy(project = model.CrossProjectName(into, s.project.crossId))
      case s => s
    }

  override def run(started: Started): Either[BleepException, Unit] = {
    val crossProjects: SortedMap[model.CrossProjectName, model.Project] =
      started.build.explodedProjects.iterator.collect { case (cp, p) if cp.name == projectName => (cp, p) }.to(SortedMap)
    val intoCrossProjects: SortedMap[model.CrossProjectName, model.Project] =
      started.build.explodedProjects.iterator.collect { case (cp, p) if cp.name == into => (cp, p) }.to(SortedMap)

    if (crossProjects.isEmpty)
      return Left(new BleepException.Text(s"Project ${projectName.value} does not exist"))
    if (intoCrossProjects.isEmpty)
      return Left(new BleepException.Text(s"Project ${into.value} does not exist"))

    if (crossProjects.size != intoCrossProjects.size)
      return Left(
        new BleepException.Text(
          s"Can only merge projects with same number of crossIds: ${projectName.value}: ${crossProjects.keys} vs ${into.value} ${intoCrossProjects.keys}"
        )
      )

    // can likely make this work, but should write some careful tests first
    (crossProjects.values.flatMap(_.`source-layout`), intoCrossProjects.values.flatMap(_.`source-layout`)) match {
      case (left, right) if left != right =>
        return Left(
          new BleepException.Text(
            s"Can only merge projects with same source layout: ${projectName.value}: ${left.mkString(", ")} vs ${into.value} ${right.mkString(", ")}"
          )
        )
      case _ => ()
    }

    val rewrittenBuild = {
      val build0 = rewrite(started.build.requireFileBacked(ctx = "command project-merge-into"), started.buildPaths)
      val build1 = build0.copy(file = build0.file.copy(scripts = build0.file.scripts.map { case (sn, sd) => (sn, sd.map(rewriteScriptDef)) }))
      BuildReapplyTemplates.normalize(build1, started.buildPaths)
    }

    def pathsFor(crossName: model.CrossProjectName): List[Path] = {
      val projectPaths = started.projectPaths(crossName)
      (projectPaths.sourcesDirs.fromSourceLayout.iterator ++ projectPaths.resourcesDirs.fromSourceLayout.iterator).toList
    }

    val b = Map.newBuilder[Path, Path]
    val errors = List.newBuilder[String]
    crossProjects.zip(intoCrossProjects).foreach { case ((crossName, _), (crossNameInto, _)) =>
      pathsFor(crossName).zip(pathsFor(crossNameInto)).foreach { case (fromSrc, intoSrc) =>
        if (Files.exists(fromSrc)) {
          Files.walk(fromSrc).forEach { fromPath =>
            if (Files.isRegularFile(fromPath)) {
              println(fromPath)
              val intoPath = intoSrc.resolve(fromSrc.relativize(fromPath))
              if (Files.exists(intoPath))
                errors += s"Expected to move project ${projectName.value} from $fromPath to $intoPath, but it already exists"
              b += fromPath -> intoPath
            }
          }
        }
      }
    }

    errors.result() match {
      case Nil =>
        Right(
          commit(
            started.logger,
            started.buildPaths,
            filesToMove = b.result().to(SortedMap),
            rewrittenBuild = rewrittenBuild.file
          )
        )
      case nonEmpty => Left(new bleep.BleepException.Text(nonEmpty.mkString("\n")))
    }
  }
}
