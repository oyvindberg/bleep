package bleep.analysis

import bleep.{KspIncrementalState, SymbolProcessorResult}
import bleep.bsp.{Outcome, ProcessRunner}
import bleep.bsp.Outcome.RunOutcome
import bleep.bsp.protocol.KillReason
import cats.effect.IO
import ryddig.Logger

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Forks a JVM running `com.google.devtools.ksp.cmdline.KSPJvmMain` against the standalone KSP runner classpath (`symbol-processing-aa-embeddable` +
  * transitive). KSP processes the project's sources, emits generated `.kt`/`.java`/`.class`/resources under the configured output directories, and exits.
  *
  * Why a separate JVM rather than in-process: the runner bundles its own kotlinc analysis-API and IntelliJ-platform classes (everything under `ksp/com/...`,
  * `ksp/org/jetbrains/...` packages inside the embeddable JAR). Loading it into bleep-bsp's existing classloader hierarchy clashes with bleep's own kotlinc. A
  * separate process keeps the two compilers isolated.
  *
  * Process lifecycle and cancellation are delegated to [[ProcessRunner.runWithOutput]] + [[Outcome.fromCancellationToken]] — the same primitives the JS /
  * Native runners use. No hand-rolled drainer thread or polling loop here.
  */
object KspRunner {

  sealed trait RunResult
  object RunResult {
    case object Success extends RunResult
    case class Failure(exitCode: Int, output: String) extends RunResult
    case object Cancelled extends RunResult
  }

  def run(
      ksp: SymbolProcessorResult,
      decision: KspIncrementalState.Decision,
      javaBin: Path,
      maxMemory: Option[String],
      cancellation: CancellationToken,
      logger: Logger
  ): IO[RunResult] =
    // Bail before forking if the build is already being cancelled. A KSP fork is ~150MB resident — not worth spinning one up just to destroy it.
    if (cancellation.isCancelled) IO.pure(RunResult.Cancelled)
    else
      IO.blocking {
        // Create output dirs eagerly. KSP will write into them; missing parents otherwise produce confusing IOException stack traces from inside Analysis API.
        List(ksp.kotlinOutputDir, ksp.javaOutputDir, ksp.classOutputDir, ksp.resourceOutputDir, ksp.cachesDir).foreach(Files.createDirectories(_))

        val cmd = List.newBuilder[String]
        cmd += javaBin.toString
        // Heap cap follows the user-level `bspServer.kspRunnerMaxMemory` setting (None = JVM default). KSP's bundled IntelliJ-platform code is memory-hungry on
        // real builds but tiny on toy fixtures — sized at the call site.
        maxMemory.foreach(m => cmd += s"-Xmx$m")
        // Suppress the runner's noisy `sun.misc.Unsafe::objectFieldOffset` deprecation warnings on JDK 21+. JetBrains' own launcher uses these flags too.
        cmd += "--add-opens=java.base/java.util=ALL-UNNAMED"
        cmd += "--add-opens=java.base/java.lang=ALL-UNNAMED"
        cmd += "-cp"
        cmd += ksp.runnerClasspath.iterator.map(_.toString).mkString(java.io.File.pathSeparator)
        cmd += "com.google.devtools.ksp.cmdline.KSPJvmMain"
        ksp.runnerArgs(decision).foreach(cmd += _)
        val cmdLine = cmd.result()

        logger.debug(s"KSP cmd: ${cmdLine.mkString(" \\\n  ")}")
        logger.debug(s"KSP runner classpath (${ksp.runnerClasspath.size} jars): ${ksp.runnerClasspath.iterator.map(_.getFileName.toString).mkString(", ")}")
        logger.debug(s"KSP processor classpath (${ksp.processorJars.size} jars): ${ksp.processorJars.iterator.map(_.getFileName.toString).mkString(", ")}")
        logger.debug(s"KSP libraries (${ksp.librariesClasspath.size} jars): ${ksp.librariesClasspath.iterator.map(_.getFileName.toString).mkString(", ")}")

        new ProcessBuilder(cmdLine.asJava).redirectErrorStream(true)
      }.flatMap { pb =>
        // Bridge: CancellationToken → Deferred[IO, KillReason]. ProcessRunner.runWithOutput races the process against this Deferred and destroyForciblies the
        // child if the Deferred fires. Output is captured up to `ProcessRunner.MaxOutputLines` (50k lines), more than enough for KSP's normal "a few warnings
        // and processor logs" output.
        Outcome.fromCancellationToken(cancellation).flatMap { kill =>
          ProcessRunner.runWithOutput(pb, kill).map { outcome =>
            def combined(stdout: String, stderr: String): String =
              if (stdout.nonEmpty && stderr.nonEmpty) s"$stdout\n$stderr" else stdout + stderr
            outcome match {
              case RunOutcome.Killed(_, _, _)              => RunResult.Cancelled
              case RunOutcome.Completed(0, stdout, stderr) =>
                val out = combined(stdout, stderr)
                if (out.nonEmpty) logger.debug(s"KSP output:\n$out")
                RunResult.Success
              case RunOutcome.Completed(exitCode, stdout, stderr)  => RunResult.Failure(exitCode, combined(stdout, stderr))
              case RunOutcome.Crashed(_, exitCode, stdout, stderr) => RunResult.Failure(exitCode, combined(stdout, stderr))
            }
          }
        }
      }
}
