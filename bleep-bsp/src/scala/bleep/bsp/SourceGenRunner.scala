package bleep.bsp

import bleep.*
import bleep.bsp.Outcome.KillReason
import bleep.internal.jvmRunCommand
import bleep.model.{CrossProjectName, ScriptDef}
import cats.effect.{Deferred, IO}

import java.nio.file.{Files, Path}
import java.time.Instant
import scala.collection.mutable

/** Runs sourcegen scripts before compilation.
  *
  * This integrates source generation into bleep-bsp's compilation pipeline:
  *   - Detects which projects have sourcegen scripts
  *   - Compiles script projects (and their dependencies)
  *   - Runs scripts to generate sources
  *   - Uses timestamp-based invalidation (like DoSourceGen in bleep-core)
  */
object SourceGenRunner {

  /** Listener for sourcegen progress events */
  trait SourceGenListener {
    def onScriptStarted(scriptMain: String, forProjects: List[String]): Unit
    def onScriptFinished(scriptMain: String, success: Boolean, durationMs: Long, error: Option[String]): Unit
    def onLog(message: String, isError: Boolean): Unit
  }

  object SourceGenListener {
    val noop: SourceGenListener = new SourceGenListener {
      def onScriptStarted(scriptMain: String, forProjects: List[String]): Unit = ()
      def onScriptFinished(scriptMain: String, success: Boolean, durationMs: Long, error: Option[String]): Unit = ()
      def onLog(message: String, isError: Boolean): Unit = ()
    }
  }

  case class ScriptToRun(
      script: ScriptDef.Main,
      forProjects: Set[CrossProjectName]
  )

  case class SourceGenResult(
      scriptsRun: Int,
      scriptsSkipped: Int,
      failures: List[String]
  ) {
    def isSuccess: Boolean = failures.isEmpty
  }

  /** Find all sourcegen scripts for the given projects.
    *
    * @param started
    *   the loaded build
    * @param projects
    *   projects being compiled
    * @return
    *   map of script -> projects that use it
    */
  def findScripts(
      started: Started,
      projects: Set[CrossProjectName]
  ): Map[ScriptDef.Main, Set[CrossProjectName]] = {
    val result = mutable.Map.empty[ScriptDef.Main, Set[CrossProjectName]]

    projects.foreach { projectName =>
      started.build.explodedProjects.get(projectName).foreach { project =>
        project.sourcegen.values.foreach { case script: ScriptDef.Main =>
          val existing = result.getOrElse(script, Set.empty)
          result(script) = existing + projectName
        }
      }
    }

    result.toMap
  }

  /** Get all script projects and their transitive dependencies.
    *
    * These need to be compiled before running scripts.
    */
  def scriptProjectsWithDeps(
      started: Started,
      scripts: Iterable[ScriptDef.Main]
  ): Set[CrossProjectName] = {
    val scriptProjects = scripts.map(_.project).toSet
    val allDeps = scriptProjects.flatMap { scriptProject =>
      started.build.transitiveDependenciesFor(scriptProject).keySet + scriptProject
    }
    allDeps
  }

  /** Check if a script needs to run based on timestamp comparison.
    *
    * A script needs to run if:
    *   - Any output directory doesn't exist
    *   - Any input file is newer than the most recent output
    *
    * @param started
    *   the loaded build
    * @param script
    *   the script to check
    * @param forProjects
    *   projects that use this script
    * @return
    *   projects that need regeneration
    */
  def projectsNeedingRegeneration(
      started: Started,
      script: ScriptDef.Main,
      forProjects: Set[CrossProjectName]
  ): Set[CrossProjectName] = {
    // Get input files: script project sources + dependencies
    val scriptProjectWithDeps = started.build.transitiveDependenciesFor(script.project).keySet + script.project
    val inputPaths: Array[Path] = scriptProjectWithDeps.flatMap { projectName =>
      val paths = started.projectPaths(projectName)
      paths.sourcesDirs.all ++ paths.resourcesDirs.all
    }.toArray

    val mostRecentInput = mostRecentFile(inputPaths)

    mostRecentInput match {
      case None =>
        // No input files found - skip
        Set.empty

      case Some(inputTime) =>
        forProjects.filter { projectName =>
          val projectPaths = started.projectPaths(projectName)
          val outputPaths = Array(
            projectPaths.sourcesDirs.generated.get(script),
            projectPaths.resourcesDirs.generated.get(script)
          ).flatten

          val mostRecentOutput = mostRecentFile(outputPaths)

          mostRecentOutput match {
            case None =>
              // Output doesn't exist - need to run
              true
            case Some(outputTime) =>
              // Input newer than output - need to run
              outputTime.isBefore(inputTime)
          }
        }
    }
  }

  private def mostRecentFile(paths: Array[Path]): Option[Instant] = {
    val allFiles = paths
      .filter(_.toFile.exists())
      .flatMap { p =>
        if (Files.isDirectory(p)) {
          import scala.jdk.CollectionConverters.*
          Files.walk(p).filter(Files.isRegularFile(_)).iterator().asScala.toArray
        } else {
          Array(p)
        }
      }

    if (allFiles.isEmpty) None
    else Some(allFiles.map(p => Files.getLastModifiedTime(p).toInstant).max)
  }

  /** Run sourcegen scripts.
    *
    * This:
    *   1. Compiles script projects via the provided compile function
    *   2. Runs each script that needs regeneration
    *   3. Returns result indicating success/failure
    *
    * @param started
    *   the loaded build
    * @param scripts
    *   scripts to run and their target projects
    * @param compileProjects
    *   function to compile a set of projects
    * @param killSignal
    *   signal to abort
    * @param listener
    *   for progress events
    * @return
    *   result of source generation
    */
  def runScripts(
      started: Started,
      scripts: Map[ScriptDef.Main, Set[CrossProjectName]],
      compileProjects: Set[CrossProjectName] => IO[Boolean],
      killSignal: Deferred[IO, KillReason],
      listener: SourceGenListener
  ): IO[SourceGenResult] =
    if (scripts.isEmpty) {
      IO.pure(SourceGenResult(0, 0, Nil))
    } else {
      // Filter to scripts that need to run
      val scriptsNeedingRun = scripts.flatMap { case (script, forProjects) =>
        val projectsNeedingRegen = projectsNeedingRegeneration(started, script, forProjects)
        if (projectsNeedingRegen.nonEmpty) {
          Some(ScriptToRun(script, projectsNeedingRegen))
        } else {
          None
        }
      }.toList

      if (scriptsNeedingRun.isEmpty) {
        listener.onLog("All sourcegen outputs up to date, skipping", false)
        IO.pure(SourceGenResult(0, scripts.size, Nil))
      } else {
        // Compile script projects first
        val allScriptProjects = scriptProjectsWithDeps(started, scriptsNeedingRun.map(_.script))

        for {
          compileSuccess <- compileProjects(allScriptProjects)
          result <-
            if (!compileSuccess) {
              IO.pure(SourceGenResult(0, 0, List("Failed to compile sourcegen script projects")))
            } else {
              // Run each script
              runScriptsSequentially(started, scriptsNeedingRun, killSignal, listener).map { failures =>
                SourceGenResult(scriptsNeedingRun.size, scripts.size - scriptsNeedingRun.size, failures)
              }
            }
        } yield result
      }
    }

  private def runScriptsSequentially(
      started: Started,
      scripts: List[ScriptToRun],
      killSignal: Deferred[IO, KillReason],
      listener: SourceGenListener
  ): IO[List[String]] =
    scripts.foldLeft(IO.pure(List.empty[String])) { case (accIO, scriptToRun) =>
      accIO.flatMap { acc =>
        runSingleScript(started, scriptToRun, killSignal, listener).map {
          case None        => acc
          case Some(error) => acc :+ error
        }
      }
    }

  /** Delete `.sourcegen-stamp` files from generated dirs so that timestamp-based staleness detection will re-run the script next time. */
  private def deleteStampFiles(
      started: Started,
      script: ScriptDef.Main,
      forProjects: Set[CrossProjectName]
  ): Unit =
    forProjects.foreach { projectName =>
      val projectPaths = started.projectPaths(projectName)
      val dirs = Array(
        projectPaths.sourcesDirs.generated.get(script),
        projectPaths.resourcesDirs.generated.get(script)
      ).flatten

      dirs.foreach { dir =>
        val stamp = dir.resolve(".sourcegen-stamp")
        Files.deleteIfExists(stamp): Unit
      }
    }

  /** Run a single sourcegen script by forking a separate JVM.
    *
    * Forks the script into its own JVM process to avoid classloader conflicts between the BSP server's classpath (which includes zinc/scala-compiler transitive
    * deps like scala-parser-combinators_2.13) and the script's own classpath (which may include scala-parser-combinators_3). The forked JVM runs
    * `BleepCodegenScript.main()` which calls `bootstrap.forScript()` to load the build and execute the script.
    */
  private def runSingleScript(
      started: Started,
      scriptToRun: ScriptToRun,
      killSignal: Deferred[IO, KillReason],
      listener: SourceGenListener
  ): IO[Option[String]] = {
    val script = scriptToRun.script
    val forProjectNames = scriptToRun.forProjects.map(_.value).toList
    val startTime = System.currentTimeMillis()

    listener.onScriptStarted(script.main, forProjectNames)
    listener.onLog(s"Running sourcegen: ${script.main} for ${forProjectNames.mkString(", ")}", false)

    buildClasspath(started, script) match {
      case Left(error) =>
        val durationMs = System.currentTimeMillis() - startTime
        listener.onScriptFinished(script.main, success = false, durationMs, Some(error))
        IO.pure(Some(error))

      case Right(cp) =>
        // Build the forked JVM command: java -cp <classpath> <script.main> -d <buildDir> -p <project1> -p <project2> ...
        val projectArgs = scriptToRun.forProjects.toList.flatMap(p => List("-p", p.value))
        val dirArgs = List("-d", started.buildPaths.buildDir.toString)
        val cmd = jvmRunCommand.cmd(started.resolvedJvm.forceGet, Nil, cp, script.main, dirArgs ++ projectArgs)

        val pb = new ProcessBuilder(cmd*)
        pb.directory(started.buildPaths.buildDir.toFile)

        ProcessRunner.runWithOutput(pb, killSignal).map { outcome =>
          val durationMs = System.currentTimeMillis() - startTime
          outcome match {
            case Outcome.RunOutcome.Completed(0, stdout, _) =>
              // Log script stdout if non-empty
              if (stdout.nonEmpty) {
                stdout.split("\n").foreach(line => listener.onLog(line, false))
              }
              listener.onLog(s"Sourcegen ${script.main} completed successfully", false)
              listener.onScriptFinished(script.main, success = true, durationMs, None)
              None

            case Outcome.RunOutcome.Completed(exitCode, stdout, stderr) =>
              deleteStampFiles(started, script, scriptToRun.forProjects)
              if (stdout.nonEmpty) {
                stdout.split("\n").foreach(line => listener.onLog(line, false))
              }
              val errorDetail = if (stderr.nonEmpty) stderr else s"exit code $exitCode"
              val error = s"Sourcegen ${script.main} failed: $errorDetail"
              listener.onLog(error, true)
              listener.onScriptFinished(script.main, success = false, durationMs, Some(error))
              Some(error)

            case Outcome.RunOutcome.Crashed(signal, _, stdout, stderr) =>
              deleteStampFiles(started, script, scriptToRun.forProjects)
              if (stdout.nonEmpty) {
                stdout.split("\n").foreach(line => listener.onLog(line, false))
              }
              val errorDetail = if (stderr.nonEmpty) stderr else s"signal $signal"
              val error = s"Sourcegen ${script.main} crashed: $errorDetail"
              listener.onLog(error, true)
              listener.onScriptFinished(script.main, success = false, durationMs, Some(error))
              Some(error)

            case Outcome.RunOutcome.Killed(reason, _, _) =>
              deleteStampFiles(started, script, scriptToRun.forProjects)
              val error = s"Sourcegen ${script.main} killed: $reason"
              listener.onLog(error, true)
              listener.onScriptFinished(script.main, success = false, durationMs, Some(error))
              Some(error)
          }
        }
    }
  }

  /** Build classpath for running a script project in a forked JVM. */
  private def buildClasspath(
      started: Started,
      script: ScriptDef.Main
  ): Either[String, List[Path]] =
    started.resolvedProjects.get(script.project) match {
      case None =>
        Left(s"Script project ${script.project.value} not found in build")

      case Some(lazyResolved) =>
        val resolved = lazyResolved.forceGet
        Right(fixedClasspath(resolved))
    }
}
