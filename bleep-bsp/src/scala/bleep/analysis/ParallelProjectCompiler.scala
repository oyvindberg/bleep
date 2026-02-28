package bleep.analysis

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import java.nio.file.Path

/** Parallel project compiler using Cats Effect.
  *
  * Compiles projects in dependency order with maximum parallelism:
  *   - Projects compile as soon as all dependencies complete
  *   - Uses signal-based scheduling: each completion immediately unblocks new tasks
  *   - Supports cancellation and failure propagation
  */
object ParallelProjectCompiler {

  /** Build progress listener for per-project compile events */
  trait BuildProgressListener {
    def onProjectStarted(projectName: String): IO[Unit]
    def onProjectFinished(projectName: String, result: ProjectCompileResult): IO[Unit]
  }

  object BuildProgressListener {
    val noop: BuildProgressListener = new BuildProgressListener {
      def onProjectStarted(projectName: String): IO[Unit] = IO.unit
      def onProjectFinished(projectName: String, result: ProjectCompileResult): IO[Unit] = IO.unit
    }
  }

  /** Result of building all projects */
  sealed trait BuildResult {
    def isSuccess: Boolean = this match {
      case _: BuildSuccess => true
      case _: BuildFailure => false
    }
  }
  case class BuildSuccess(results: Map[String, ProjectCompileSuccess]) extends BuildResult
  case class BuildFailure(
      completed: Map[String, ProjectCompileSuccess],
      failed: Map[String, ProjectCompileFailure],
      notStarted: Set[String]
  ) extends BuildResult

  /** Build all projects in the DAG with parallel execution.
    *
    * @param dag
    *   the project dependency graph
    * @param parallelism
    *   maximum number of concurrent compilations
    * @param diagnosticListener
    *   receives compilation diagnostics
    * @param cancellationToken
    *   for cancellation support
    * @param progressListener
    *   receives per-project start/finish events
    * @return
    *   build result (success or failure with details)
    */
  def build(
      dag: ProjectDag,
      parallelism: Int,
      diagnosticListener: DiagnosticListener,
      cancellationToken: CancellationToken,
      progressListener: BuildProgressListener
  ): IO[BuildResult] =
    dag.validate match {
      case Left(err) =>
        IO.pure(
          BuildFailure(
            Map.empty,
            Map(
              err.toString -> ProjectCompileFailure(
                List(
                  CompilerError(None, 0, 0, s"DAG validation failed: $err", None, CompilerError.Severity.Error)
                )
              )
            ),
            dag.projects.keySet
          )
        )
      case Right(_) =>
        buildValidDag(dag, parallelism, diagnosticListener, cancellationToken, progressListener)
    }

  private def buildValidDag(
      dag: ProjectDag,
      parallelism: Int,
      diagnosticListener: DiagnosticListener,
      cancellationToken: CancellationToken,
      progressListener: BuildProgressListener
  ): IO[BuildResult] =
    for {
      // State: completed results
      completedRef <- Ref.of[IO, Map[String, ProjectCompileResult]](Map.empty)
      // Deferreds for each project - allows waiting for dependencies
      deferreds <- dag.projects.keys.toList
        .traverse { name =>
          Deferred[IO, ProjectCompileResult].map(name -> _)
        }
        .map(_.toMap)
      // Build all projects
      _ <- buildProjects(dag, parallelism, completedRef, deferreds, diagnosticListener, cancellationToken, progressListener)
      // Collect results
      completed <- completedRef.get
    } yield {
      val successes = completed.collect { case (name, s: ProjectCompileSuccess) => name -> s }
      val failures = completed.collect { case (name, f: ProjectCompileFailure) => name -> f }
      val notStarted = dag.projects.keySet -- completed.keySet

      if (failures.isEmpty && notStarted.isEmpty) {
        BuildSuccess(successes)
      } else {
        BuildFailure(successes, failures, notStarted)
      }
    }

  private def buildProjects(
      dag: ProjectDag,
      parallelism: Int,
      completedRef: Ref[IO, Map[String, ProjectCompileResult]],
      deferreds: Map[String, Deferred[IO, ProjectCompileResult]],
      diagnosticListener: DiagnosticListener,
      cancellationToken: CancellationToken,
      progressListener: BuildProgressListener
  ): IO[Unit] = {
    // Signal-based scheduling: each task completion signals the loop to re-evaluate.
    // This ensures new tasks start as soon as a slot opens, not when an entire batch finishes.
    def loop(
        runningRef: Ref[IO, Set[String]],
        signalRef: Ref[IO, Deferred[IO, Unit]]
    ): IO[Unit] =
      for {
        completed <- completedRef.get
        running <- runningRef.get
        completedNames = completed.keySet
        allDone = completedNames == dag.projects.keySet
        _ <-
          if (allDone) {
            IO.unit
          } else if (cancellationToken.isCancelled) {
            val remaining = dag.projects.keySet -- completedNames -- running
            remaining.toList.traverse_ { name =>
              val result = ProjectCompileFailure(List(CompilerError(None, 0, 0, "Compilation cancelled", None, CompilerError.Severity.Error)))
              completedRef.update(_ + (name -> result)) >>
                deferreds(name).complete(result).attempt.void
            }
          } else {
            val hasFailures = completed.values.exists(!_.isSuccess)
            if (hasFailures && running.isEmpty) {
              // All running tasks done and we have failures — cancel remaining
              val remaining = dag.projects.keySet -- completedNames
              remaining.toList.traverse_ { name =>
                val result = ProjectCompileFailure(List(CompilerError(None, 0, 0, "Not started due to dependency failure", None, CompilerError.Severity.Error)))
                completedRef.update(_ + (name -> result)) >>
                  deferreds(name).complete(result).attempt.void
              }
            } else if (hasFailures) {
              // Failures exist but tasks still running — wait for them to drain
              signalRef.get.flatMap(_.get) >>
                Deferred[IO, Unit].flatMap(s => signalRef.set(s) >> loop(runningRef, signalRef))
            } else {
              val ready = dag.ready(completedNames) -- completedNames -- running
              val availableSlots = parallelism - running.size
              val toStart = ready.toList.take(availableSlots)

              // Launch each task as a fire-and-forget fiber
              toStart.traverse_ { name =>
                runningRef.update(_ + name) >>
                  (progressListener.onProjectStarted(name) >>
                    compileProject(dag.projects(name), dag, deferreds, diagnosticListener, cancellationToken).flatMap { result =>
                      progressListener.onProjectFinished(name, result) >>
                        completedRef.update(_ + (name -> result)) >>
                        deferreds(name).complete(result).attempt.void
                    })
                    .guarantee(
                      runningRef.update(_ - name) >>
                        signalRef.get.flatMap(_.complete(()).attempt.void)
                    )
                    .start
                    .void
              } >>
                // If nothing is running and nothing was started, we're stuck or done
                runningRef.get.flatMap { nowRunning =>
                  if (nowRunning.isEmpty && toStart.isEmpty) {
                    // Check if actually complete
                    completedRef.get.flatMap { nowCompleted =>
                      if (nowCompleted.keySet == dag.projects.keySet) IO.unit
                      else {
                        // Stuck — shouldn't happen with a valid DAG, but handle gracefully
                        val stuck = dag.projects.keySet -- nowCompleted.keySet
                        stuck.toList.traverse_ { name =>
                          val result = ProjectCompileFailure(List(CompilerError(None, 0, 0, "Stuck: no progress possible", None, CompilerError.Severity.Error)))
                          completedRef.update(_ + (name -> result)) >>
                            deferreds(name).complete(result).attempt.void
                        }
                      }
                    }
                  } else {
                    // Wait for any task to complete, then re-evaluate
                    signalRef.get.flatMap(_.get) >>
                      Deferred[IO, Unit].flatMap(s => signalRef.set(s) >> loop(runningRef, signalRef))
                  }
                }
            }
          }
      } yield ()

    for {
      runningRef <- Ref.of[IO, Set[String]](Set.empty)
      initialSignal <- Deferred[IO, Unit]
      signalRef <- Ref.of[IO, Deferred[IO, Unit]](initialSignal)
      _ <- loop(runningRef, signalRef)
    } yield ()
  }

  private def compileProject(
      config: ProjectConfig,
      dag: ProjectDag,
      deferreds: Map[String, Deferred[IO, ProjectCompileResult]],
      diagnosticListener: DiagnosticListener,
      cancellationToken: CancellationToken
  ): IO[ProjectCompileResult] = {
    // Wait for all dependencies to complete
    val deps = dag.dependenciesOf(config.name)
    for {
      depResults <- deps.toList.traverse(d => deferreds(d).get.map(d -> _))
      result <- {
        // Check if any dependency failed
        val depFailures = depResults.collect { case (_, f: ProjectCompileFailure) => f }
        if (depFailures.nonEmpty) {
          IO.pure(
            ProjectCompileFailure(
              List(
                CompilerError(None, 0, 0, s"Dependency failed: ${depFailures.flatMap(_.errors).map(_.message).mkString(", ")}", None, CompilerError.Severity.Error)
              )
            )
          )
        } else {
          // Collect dependency analyses (output dir -> analysis file)
          val depAnalyses: Map[Path, Path] = depResults
            .collect { case (depName, success: ProjectCompileSuccess) =>
              success.analysisFile.map(af => dag.projects(depName).outputDir -> af)
            }
            .flatten
            .toMap

          // Compile with appropriate compiler, passing dependency analyses
          val compiler = ProjectCompiler.forLanguage(config.language)
          compiler.compile(config, diagnosticListener, cancellationToken, depAnalyses, ProgressListener.noop)
        }
      }
    } yield result
  }

  /** Build a single project and its dependencies (useful for targeted builds).
    *
    * @param projectName
    *   name of the project to build
    * @param dag
    *   the project dependency graph
    * @param parallelism
    *   maximum concurrent compilations
    * @param diagnosticListener
    *   receives compilation diagnostics
    * @param cancellationToken
    *   for cancellation support
    * @return
    *   build result for the project and its dependencies
    */
  def buildProject(
      projectName: String,
      dag: ProjectDag,
      parallelism: Int,
      diagnosticListener: DiagnosticListener,
      cancellationToken: CancellationToken,
      progressListener: BuildProgressListener
  ): IO[BuildResult] = {
    // Find transitive dependencies
    def transitiveDeps(name: String, visited: Set[String] = Set.empty): Set[String] =
      if (visited.contains(name)) Set.empty
      else {
        val deps = dag.dependenciesOf(name)
        deps ++ deps.flatMap(d => transitiveDeps(d, visited + name))
      }

    val needed = transitiveDeps(projectName) + projectName
    val filteredDag = ProjectDag(
      dag.projects.view.filterKeys(needed.contains).toMap,
      dag.edges.view.filterKeys(needed.contains).mapValues(_.filter(needed.contains)).toMap
    )

    build(filteredDag, parallelism, diagnosticListener, cancellationToken, progressListener)
  }
}
