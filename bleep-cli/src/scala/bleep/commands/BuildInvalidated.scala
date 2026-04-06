package bleep
package commands

import cats.syntax.apply.*
import com.monovore.decline.Opts

import scala.collection.immutable.SortedSet
import scala.collection.mutable
import scala.util.control.NonFatal

case class BuildInvalidated(
    base: String,
    outputMode: OutputMode
) extends BleepBuildCommand {
  override def run(started: Started): Either[BleepException, Unit] = {
    val buildDir = started.buildPaths.buildDir

    // Step 1: Load base build from git
    val oldBuildStr: String =
      try
        scala.sys.process
          .Process(
            List("git", "show", s"$base:${BuildLoader.BuildFileName}"),
            buildDir.toFile
          )
          .!!
      catch {
        case NonFatal(th) =>
          throw new BleepException.Cause(th, s"couldn't load ${BuildLoader.BuildFileName} from $base")
      }

    val baseBuildFile = BuildLoader
      .Existing(started.buildPaths.bleepYamlFile, Lazy(Right(oldBuildStr)))
      .buildFile
      .forceGet
      .orThrow

    val baseBuild = model.Build.FileBacked(baseBuildFile)
    val currentBuild = started.build

    // Step 2: Config-invalidated projects
    val directlyInvalidated = mutable.Set.empty[model.CrossProjectName]

    currentBuild.explodedProjects.foreach { case (crossName, currentProject) =>
      baseBuild.explodedProjects.get(crossName) match {
        case None =>
          directlyInvalidated += crossName
        case Some(baseProject) =>
          if (currentProject != baseProject)
            directlyInvalidated += crossName
      }
    }

    // Step 3: Source-invalidated projects
    val changedFiles: Set[java.nio.file.Path] = {
      val output =
        try
          scala.sys.process
            .Process(
              List("git", "diff", "--name-only", base),
              buildDir.toFile
            )
            .!!
        catch {
          case NonFatal(th) =>
            throw new BleepException.Cause(th, s"couldn't run git diff against $base")
        }
      output.linesIterator
        .filter(_.nonEmpty)
        .map(line => buildDir.resolve(line).normalize())
        .toSet
    }

    currentBuild.explodedProjects.foreach { case (crossName, project) =>
      if (!directlyInvalidated.contains(crossName)) {
        val projectPaths = started.buildPaths.project(crossName, project)
        val allDirs = projectPaths.sourcesDirs.all ++ projectPaths.resourcesDirs.all
        val hasChangedSource = changedFiles.exists { changedFile =>
          allDirs.exists(dir => changedFile.startsWith(dir))
        }
        if (hasChangedSource)
          directlyInvalidated += crossName
      }
    }

    // Step 4: Transitive dependents
    val reverseDeps = BuildInvalidated.computeReverseDeps(currentBuild)
    val allInvalidated = BuildInvalidated.transitiveDependents(directlyInvalidated.toSet, reverseDeps)

    // Step 5: Output
    val sorted = SortedSet.empty[model.CrossProjectName] ++ allInvalidated
    outputMode match {
      case OutputMode.Json =>
        CommandResult.print(CommandResult.success(ProjectList(sorted.toList.map(_.value))))
      case OutputMode.Text =>
        sorted.foreach(n => println(n.value))
    }

    Right(())
  }
}

object BuildInvalidated {
  val base: Opts[String] = Opts.option[String](
    "base",
    "git commitish to compare against (e.g., origin/master)",
    "b"
  )

  private val jsonFlag: Opts[OutputMode] = Opts.flag("json", "output as JSON (alias for --output json)").map(_ => OutputMode.Json)

  private val outputOpt: Opts[OutputMode] =
    Opts
      .option[String]("output", "output format: text or json", "o")
      .map {
        case "json" => OutputMode.Json
        case _      => OutputMode.Text
      }

  val outputMode: Opts[OutputMode] = jsonFlag.orElse(outputOpt).withDefault(OutputMode.Text)

  val opts: Opts[BuildInvalidated] =
    (base, outputMode).mapN(BuildInvalidated.apply)

  def computeReverseDeps(
      build: model.Build
  ): Map[model.CrossProjectName, Set[model.CrossProjectName]] = {
    val builder = mutable.Map.empty[model.CrossProjectName, mutable.Set[model.CrossProjectName]]

    build.resolvedDependsOn.foreach { case (project, deps) =>
      deps.foreach { dep =>
        builder.getOrElseUpdate(dep, mutable.Set.empty) += project
      }
    }

    build.explodedProjects.foreach { case (project, p) =>
      p.sourcegen.values.foreach { case model.ScriptDef.Main(sourcegenProject, _, _) =>
        builder.getOrElseUpdate(sourcegenProject, mutable.Set.empty) += project
      }
    }

    builder.view.mapValues(_.toSet).toMap
  }

  def transitiveDependents(
      directlyInvalidated: Set[model.CrossProjectName],
      reverseDeps: Map[model.CrossProjectName, Set[model.CrossProjectName]]
  ): Set[model.CrossProjectName] = {
    val result = mutable.Set.empty[model.CrossProjectName]
    val queue = mutable.Queue.from(directlyInvalidated)
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      if (result.add(current)) {
        reverseDeps.getOrElse(current, Set.empty).foreach(queue.enqueue(_))
      }
    }
    result.toSet
  }
}
