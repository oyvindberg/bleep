package bleep.bsp

import bleep.*
import bleep.bsp.Outcome.KillReason
import bleep.model.{CrossProjectName, ScriptDef}
import cats.effect.{Deferred, IO}

import java.net.{URL, URLClassLoader}
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

  /** Delete generated source/resource directories before running a script. This ensures that if the script fails or crashes partway through, no partial output
    * remains — so timestamp-based up-to-date detection will correctly re-run it next time.
    */
  private def deleteGeneratedDirs(
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
      Files.list(path).iterator().asScala.foreach(deleteRecursively)
    }
    Files.deleteIfExists(path): Unit
  }

  /** Run a single sourcegen script by classloading it in-process.
    *
    * Instead of spawning a separate JVM, we:
    *   1. Build a URLClassLoader from the script project's compiled classes + classpath
    *   2. Load the script class and cast to BleepCodegenScript
    *   3. Call run() directly with the BSP server's Started instance
    *
    * This avoids the script needing to bootstrap itself (which would fail for build.bleep:* deps in tests).
    */
  private def runSingleScript(
      started: Started,
      scriptToRun: ScriptToRun,
      @annotation.nowarn("msg=unused") killSignal: Deferred[IO, KillReason],
      listener: SourceGenListener
  ): IO[Option[String]] = {
    val script = scriptToRun.script
    val forProjectNames = scriptToRun.forProjects.map(_.value).toList
    val startTime = System.currentTimeMillis()

    listener.onScriptStarted(script.main, forProjectNames)
    listener.onLog(s"Running sourcegen: ${script.main} for ${forProjectNames.mkString(", ")}", false)

    IO.blocking {
      buildClasspathUrls(started, script) match {
        case Left(error) =>
          val durationMs = System.currentTimeMillis() - startTime
          listener.onScriptFinished(script.main, success = false, durationMs, Some(error))
          Some(error)

        case Right(urls) =>
          val classLoader = new URLClassLoader(urls, getClass.getClassLoader)
          try {
            // Scala objects compile to a $-suffixed class with a MODULE$ static field holding the singleton
            val clazz = classLoader.loadClass(script.main + "$")
            val instance = clazz.getField("MODULE$").get(null).asInstanceOf[BleepCodegenScript]

            val targets = scriptToRun.forProjects.toList.map { projectName =>
              val sourcesDir = started.buildPaths.generatedSourcesDir(projectName, instance.ThisClassName)
              val resourcesDir = started.buildPaths.generatedResourcesDir(projectName, instance.ThisClassName)
              instance.Target(projectName, sourcesDir, resourcesDir)
            }

            instance.run(started, new Commands(started), targets, Nil)

            val durationMs = System.currentTimeMillis() - startTime
            listener.onLog(s"Sourcegen ${script.main} completed successfully", false)
            listener.onScriptFinished(script.main, success = true, durationMs, None)
            None
          } catch {
            case e: Exception =>
              deleteGeneratedDirs(started, script, scriptToRun.forProjects)
              val durationMs = System.currentTimeMillis() - startTime
              val error = s"Sourcegen ${script.main} failed: ${e.getMessage}"
              listener.onLog(error, true)
              listener.onScriptFinished(script.main, success = false, durationMs, Some(error))
              Some(error)
          } finally classLoader.close()
      }
    }
  }

  /** Build classpath URLs for classloading a script project. */
  private def buildClasspathUrls(
      started: Started,
      script: ScriptDef.Main
  ): Either[String, Array[URL]] =
    started.resolvedProjects.get(script.project) match {
      case None =>
        Left(s"Script project ${script.project.value} not found in build")

      case Some(lazyResolved) =>
        val resolved = lazyResolved.forceGet

        // Build classpath: classes + resources + dependencies
        val allPaths = List(resolved.classesDir) ++ resolved.resources.getOrElse(Nil) ++ resolved.classpath
        Right(allPaths.map(_.toUri.toURL).toArray)
    }
}
