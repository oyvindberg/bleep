package bleep
package internal

import bleep.commands.Run
import bleep.internal.compat.IteratorCompatOps
import bloop.rifle.BuildServer

import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path}
import java.time.{Duration, Instant}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.math.Ordering.Implicits.infixOrderingOps

object DoSourceGen {
  case class PathWithLastModified(path: Path, lastModified: FileTime) {
    def asString(now: Instant): String =
      s"$path (last modified ${Duration.between(lastModified.toInstant, now)} ago)"
  }

  // if we have more than one, and one appears in the dependencies of the other, run scripts in correct order
  case class TopologicalOrdering(build: model.Build) extends Ordering[model.ScriptDef] {
    override def compare(x: model.ScriptDef, y: model.ScriptDef): Int =
      (x, y) match {
        case (x: model.ScriptDef.Main, y: model.ScriptDef.Main) =>
          def xContainsY = build.transitiveDependenciesFor(x.project).contains(y.project)

          def yContainsX = build.transitiveDependenciesFor(y.project).contains(x.project)

          if (xContainsY) 1 else if (yContainsX) -1 else x.main.compareTo(y.main)
      }
  }
  case class ScriptPaths(allInputs: Array[Path], forProjectsAndInputs: Map[model.CrossProjectName, Array[Path]])

  def findScriptPaths(started: Started, projects: TransitiveProjects): Map[model.ScriptDef, ScriptPaths] = {
    val scriptGeneratesForProjects: Map[model.ScriptDef, List[model.CrossProjectName]] = {
      val b = mutable.Map.empty[model.ScriptDef, List[model.CrossProjectName]]
      projects.all.foreach { projectName =>
        started.build.explodedProjects(projectName).sourcegen.values.foreach { sourceGen =>
          b(sourceGen) = projectName :: b.getOrElse(sourceGen, Nil)
        }
      }
      b.toMap
    }

    scriptGeneratesForProjects.map { case (sourceGen: model.ScriptDef.Main, forProjects) =>
      val scriptProjectWithTransitive =
        TransitiveProjects(started.build, Array(sourceGen.project)).all

      val allInputs: Array[Path] =
        scriptProjectWithTransitive.flatMap { projectName =>
          val paths = started.projectPaths(projectName)
          paths.sourcesDirs.all.toArray ++ paths.resourcesDirs.all ++ List(started.buildPaths.bloopFile(projectName))
        }

      val analyzedForProjects = forProjects.map { projectName =>
        val projectPaths = started.projectPaths(projectName)
        val allOutputs = Array(projectPaths.sourcesDirs.generated(sourceGen), projectPaths.resourcesDirs.generated(sourceGen))

        (projectName, allOutputs)
      }.toMap

      (sourceGen, ScriptPaths(allInputs, analyzedForProjects))
    }
  }

  def apply(started: Started, bloop: BuildServer, projects: TransitiveProjects) = {
    val scriptsWithPaths = findScriptPaths(started, projects)

    val sortedScriptsWithPath = scriptsWithPaths.toList.sortBy(_._1)(TopologicalOrdering(started.build))

    def mostRecent(all: Array[Path]): Option[PathWithLastModified] = {
      def allFilesFor(allFiles: Array[Path]): Array[PathWithLastModified] =
        allFiles
          .filter(_.toFile.exists())
          .flatMap(p => if (Files.isDirectory(p)) Files.walk(p).filter(Files.isRegularFile(_)).iterator().asScala else Iterator(p))
          .map(p => PathWithLastModified(p, Files.getLastModifiedTime(p)))

      allFilesFor(all).maxByOptionCompat(_.lastModified)
    }
    val now = Instant.now()
    val toRun: List[(model.ScriptDef, Iterable[model.CrossProjectName])] =
      sortedScriptsWithPath.flatMap { case (script: model.ScriptDef.Main, scriptPaths) =>
        mostRecent(scriptPaths.allInputs) match {
          case Some(mostRecentInput) =>
            val filtered: Iterable[model.CrossProjectName] =
              scriptPaths.forProjectsAndInputs.flatMap { case (projectName, outputsForProject) =>
                def describe = s"source generator `${script.main}` for project ${projectName.value}"
                mostRecent(outputsForProject) match {
                  case Some(mostRecentOutput) =>
                    if (mostRecentOutput.lastModified < mostRecentInput.lastModified) {
                      started.logger.info(
                        s"Running $describe because most recent input ${mostRecentInput.asString(now)} is newer than most recent output ${mostRecentOutput.asString(now)}"
                      )
                      Some(projectName)
                    } else {
                      started.logger.debug(
                        s"Not Running $describe because most recent input ${mostRecentInput
                            .asString(now)} is not newer than most recent output ${mostRecentOutput.asString(now)}"
                      )
                      Nil
                    }
                  case None =>
                    started.logger.info(s"Running $describe because output didn't exist")
                    Some(projectName)
                }
              }
            if (filtered.nonEmpty) List((script, filtered)) else Nil
          case None =>
            started.logger.warn(s"Couldn't find any input files for generator $script. Skipping")
            Nil
        }
      }

    toRun.foldLeft(Right(()): Either[BleepException, Unit]) {
      case (left @ Left(_), _) => left
      case (Right(()), (main: model.ScriptDef.Main, forProjects)) =>
        Run(
          main.project,
          Some(main.main),
          forProjects.flatMap(x => List("--project", x.value)).toList,
          raw = false,
          watch = false
        ).runWithServer(started, bloop)
    }
  }
}
