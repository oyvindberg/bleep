package bleep.bsp

import bleep.*
import bleep.bsp.protocol.KillReason
import bleep.internal.jvmRunCommand
import bleep.model.{CrossProjectName, ScriptDef}
import cats.effect.{Deferred, IO}

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}
import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
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

  /** Newest `lastModifiedTime` across the given paths (recursing into directories).
    *
    * Streams via `Files.walkFileTree` rather than materializing an `Array[Path]` then mapping — for a source dir with 50k files that saves the path list +
    * intermediate Java/Scala iterator wrappers + the redundant `Files.getLastModifiedTime` call (the visitor already has the BasicFileAttributes from the OS
    * stat). Also avoids `Files.walk`'s `Stream` (no `Using` block needed; FileVisitor doesn't hold any FDs across calls).
    */
  private def mostRecentFile(paths: Array[Path]): Option[Instant] = {
    var best: Instant = null
    def offer(t: Instant): Unit = if (best == null || t.isAfter(best)) best = t

    val visitor = new SimpleFileVisitor[Path] {
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        if (attrs.isRegularFile) offer(attrs.lastModifiedTime.toInstant)
        FileVisitResult.CONTINUE
      }
    }

    paths.foreach { p =>
      if (Files.exists(p)) {
        if (Files.isDirectory(p)) Files.walkFileTree(p, visitor): Unit
        else offer(Files.getLastModifiedTime(p).toInstant)
      }
    }
    Option(best)
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

  /** Per-script semaphores to prevent concurrent runs of the same sourcegen script.
    *
    * When multiple BSP operations (compile, test) run concurrently and both need the same sourcegen script, the semaphore ensures only one runs it. The second
    * waiter re-checks timestamps under the semaphore and skips if the first run already produced fresh outputs.
    *
    * Uses cats-effect Semaphore instead of ReentrantLock because IO fibers can switch threads between lock/unlock, causing IllegalMonitorStateException with
    * ReentrantLock.
    *
    * Keyed on `(scriptProject, mainClass)` rather than `mainClass` alone — two different script projects can legitimately share a main-class name (e.g. both
    * declare a `scripts.Generate`), and serializing across project boundaries because of that coincidence would block independent work.
    */
  private case class ScriptKey(scriptProject: CrossProjectName, mainClass: String)

  private val scriptSemaphores = new ConcurrentHashMap[ScriptKey, cats.effect.std.Semaphore[IO]]()

  private def getScriptSemaphore(key: ScriptKey): IO[cats.effect.std.Semaphore[IO]] =
    IO.delay(scriptSemaphores.get(key)).flatMap {
      case null =>
        cats.effect.std.Semaphore[IO](1).flatMap { sem =>
          IO.delay {
            val existing = scriptSemaphores.putIfAbsent(key, sem)
            if (existing != null) existing else sem
          }
        }
      case existing => IO.pure(existing)
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

  /** Delete generated source/resource directories on failure.
    *
    * When sourcegen fails, stale generated sources from a previous successful run can cause confusing compilation errors on the next build attempt (e.g. Java
    * `record` keyword errors when old generated Java sources get fed to kotlinc). Deleting the directories ensures the next build either re-runs sourcegen or
    * fails cleanly with "no sources".
    */
  private def deleteGeneratedSources(
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
        if (Files.exists(dir)) {
          deleteRecursively(dir)
        }
      }
    }

  private def deleteRecursively(path: Path): Unit = {
    import scala.jdk.CollectionConverters.*
    if (Files.isDirectory(path)) {
      val stream = Files.list(path)
      try stream.iterator().asScala.foreach(deleteRecursively)
      finally stream.close()
    }
    Files.deleteIfExists(path): Unit
  }

  /** Run a single sourcegen script as a discrete step (caller provides the script project + its deps already compiled).
    *
    * Timestamp-based short-circuit: if all target projects are already up-to-date relative to script inputs, returns `None` without forking.
    *
    * Guarded by a per-script semaphore so two concurrent DAG executions that happen to share a script don't fork it twice. When the second waiter gets the
    * semaphore it re-checks timestamps — if the first caller already produced fresh outputs, the second skips.
    *
    * Returns `None` on success, `Some(error)` on failure. Never throws.
    */
  def runOne(
      started: Started,
      script: ScriptDef.Main,
      forProjects: Set[CrossProjectName],
      killSignal: Deferred[IO, KillReason],
      listener: SourceGenListener
  ): IO[Option[String]] =
    getScriptSemaphore(ScriptKey(script.project, script.main)).flatMap { sem =>
      sem.permit.use { _ =>
        val stillNeeded = projectsNeedingRegeneration(started, script, forProjects)
        if (stillNeeded.isEmpty) {
          listener.onLog(s"Sourcegen ${script.main} already up to date", false)
          IO.pure(None)
        } else {
          runSingleScriptLocked(started, ScriptToRun(script, stillNeeded), killSignal, listener)
        }
      }
    }

  /** Legacy per-script entry point (kept for `runScripts` callers). */
  private def runSingleScript(
      started: Started,
      scriptToRun: ScriptToRun,
      killSignal: Deferred[IO, KillReason],
      listener: SourceGenListener
  ): IO[Option[String]] =
    runOne(started, scriptToRun.script, scriptToRun.forProjects, killSignal, listener)

  /** Run a single sourcegen script by forking a separate JVM. Caller must hold the script lock.
    *
    * Forks the script into its own JVM process to avoid classloader conflicts between the BSP server's classpath (which includes zinc/scala-compiler transitive
    * deps like scala-parser-combinators_2.13) and the script's own classpath (which may include scala-parser-combinators_3). The forked JVM runs
    * `BleepCodegenScript.main()` which calls `bootstrap.forScript()` to load the build and execute the script.
    */
  private def runSingleScriptLocked(
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
        val projectArgs = scriptToRun.forProjects.toList.flatMap(p => List("-p", p.value))
        val dirArgs = List("-d", started.buildPaths.buildDir.toString)
        // Always bound the fork. An unstated -Xmx means HotSpot gives this script a quarter of the
        // machine, which the governor has already reserved against on the assumption it does not.
        val jvmOptions = List(s"-Xmx${bleep.MachineResources.forkHeapMb(started.config.bspServerConfigOrDefault.sourcegenMaxMemory)}m")
        val cmd = jvmRunCommand.cmd(started.resolvedJvm.forceGet, jvmOptions, cp, script.main, dirArgs ++ projectArgs)

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
              deleteGeneratedSources(started, script, scriptToRun.forProjects)
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
              deleteGeneratedSources(started, script, scriptToRun.forProjects)
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
              deleteGeneratedSources(started, script, scriptToRun.forProjects)
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
