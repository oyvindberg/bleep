package bleep
package commands

import cats.syntax.apply.*
import com.monovore.decline.Opts

import scala.collection.immutable.SortedSet
import scala.collection.mutable
import scala.util.control.NonFatal

case class BuildInvalidated(
    /** The ref to compare against. `None` means "no `--base` given" and resolves to [[BuildInvalidated.resolveBase]]'s default. */
    base: Option[String],
    outputMode: OutputMode
) extends BleepBuildCommand {
  override def run(started: Started): Either[BleepException, Unit] = {
    val sorted = BuildInvalidated.compute(started, BuildInvalidated.resolveBase(started, base))
    outputMode match {
      case OutputMode.Json =>
        CommandResult.print(CommandResult.success(ProjectList(sorted.toList.map(_.value))))
      case OutputMode.Text | OutputMode.Raw =>
        sorted.foreach(n => println(n.value))
    }

    Right(())
  }
}

object BuildInvalidated {
  val base: Opts[Option[String]] = Opts
    .option[String](
      "base",
      "git commitish to compare against (e.g., origin/master). Defaults to this branch's upstream",
      "b"
    )
    .orNone

  /** The ref an invalidation compares against. An explicit `--base` / `--invalidated=<ref>` wins; otherwise we ask git what this branch is based on —
    * `@{upstream}`, git's own answer, not a guessed `origin/main`.
    *
    * No upstream (detached HEAD, a branch that was never pushed, a CI checkout of a bare ref) means git has no answer, and neither do we: this throws rather
    * than quietly picking a ref and building the wrong set of projects.
    */
  def resolveBase(started: Started, explicit: Option[String]): String =
    explicit match {
      case Some(ref) => ref
      case None      =>
        val out = new StringBuilder
        val exitCode = scala.sys.process
          .Process(List("git", "rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{upstream}"), started.buildPaths.buildDir.toFile)
          .!(scala.sys.process.ProcessLogger(line => out.append(line), _ => ()))
        val upstream = out.toString.trim
        if (exitCode != 0 || upstream.isEmpty)
          throw new BleepException.Text(
            "no base ref to compare against: this branch has no upstream (`git rev-parse @{upstream}` found none). " +
              "Name one explicitly, for instance `--base origin/main` or `--invalidated=origin/main`."
          )
        else upstream
    }

  /** The projects a diff against `base` invalidated: their config changed in `bleep.yaml`, a file under their sources/resources changed, or they transitively
    * depend on one that did. Shared by `bleep build invalidated`, `bleep compile --invalidated`, `bleep test --invalidated` and `bleep ci --invalidated`, so
    * the listed set and the built set can never diverge.
    *
    * ==Build-level fields==
    *
    * `jvm:` and `$version` belong to the build, not to any project, so they appear in no project's exploded config and the per-project comparison below cannot
    * see them. Left at that, bumping the toolchain JDK — the compiler that produces every class file in the build — reported an empty invalidated set, and a
    * first-class `--invalidated` flag that misses a toolchain change is worse than the `xargs` pipeline it replaces, because it looks authoritative. Either
    * changing therefore invalidates every project.
    *
    * ==Why `resolvers:` is deliberately NOT one of them==
    *
    * Do not "fix" this by adding it. A Maven coordinate is expected to resolve to identical bytes whichever repository serves it, and if an artifact is missing
    * from the configured repositories then resolution fails hard before anything is compiled — there is no path where a changed resolver list silently produces
    * different output. Treating resolvers as invalidating would, on the other hand, rebuild every project in the build the moment someone adds a repository for
    * one new dependency, buying nothing in return. The one case it would catch — two repositories serving different bytes under one coordinate — is a
    * supply-chain incident, not a build-selection problem. This mirrors, deliberately, the same decision documented in `ProjectDigest`.
    */
  def compute(started: Started, base: String): SortedSet[model.CrossProjectName] = {
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

    val directlyInvalidated = mutable.Set.empty[model.CrossProjectName]

    // Step 2a: build-level changes invalidate everything (see the class comment for why these fields and not `resolvers:`)
    val buildLevelChange: Option[String] =
      if (baseBuild.jvm != currentBuild.jvm)
        Some(s"toolchain jvm: ${describeJvm(baseBuild.jvm)} -> ${describeJvm(currentBuild.jvm)}")
      else if (baseBuild.$version != currentBuild.$version)
        Some(s"bleep $$version: ${baseBuild.$version.value} -> ${currentBuild.$version.value}")
      else None

    buildLevelChange.foreach { what =>
      started.logger.info(s"$what — every project is invalidated")
      directlyInvalidated ++= currentBuild.explodedProjects.keys
    }

    // Step 2b: Config-invalidated projects
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
        val allDirs = ProjectInputs.all(project, projectPaths)
        val hasChangedSource = changedFiles.exists { changedFile =>
          allDirs.exists(dir => changedFile.startsWith(dir))
        }
        if (hasChangedSource)
          directlyInvalidated += crossName
      }
    }

    // Step 4: Transitive dependents
    val reverseDeps = computeReverseDeps(currentBuild)
    SortedSet.empty[model.CrossProjectName] ++ transitiveDependents(directlyInvalidated.toSet, reverseDeps)
  }

  private val jsonFlag: Opts[OutputMode] = Opts.flag("json", "output as JSON (alias for --output json)").map(_ => OutputMode.Json)

  private val outputOpt: Opts[OutputMode] =
    Opts
      .option[String]("output", "output format: text, json, or raw", "o")
      .map {
        case "json" => OutputMode.Json
        case "raw"  => OutputMode.Raw
        case _      => OutputMode.Text
      }

  val outputMode: Opts[OutputMode] = jsonFlag.orElse(outputOpt).withDefault(OutputMode.Text)

  val opts: Opts[BuildInvalidated] =
    (base, outputMode).mapN(BuildInvalidated.apply)

  /** `jvm.index` is part of the identity: the index is what maps a name like `temurin:21` to an actual distribution, so the same name under a different index
    * is a different JDK.
    */
  private def describeJvm(jvm: Option[model.Jvm]): String =
    jvm match {
      case None      => "<none>"
      case Some(jvm) => jvm.index.fold(jvm.name)(index => s"${jvm.name} (index $index)")
    }

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
