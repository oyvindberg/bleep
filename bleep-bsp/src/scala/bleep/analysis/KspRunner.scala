package bleep.analysis

import bleep.{KspIncrementalState, SymbolProcessorResult}
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
  * The runner reads source files directly — no compile-before-KSP step is required for the project itself. Upstream-project class dirs must be on
  * [[SymbolProcessorResult.librariesClasspath]] so KSP can resolve cross-project types; that's the caller's responsibility (see `RunSymbolProcessorsTask`
  * dependencies in `TaskDag`).
  */
object KspRunner {

  sealed trait RunResult
  object RunResult {
    case object Success extends RunResult
    case class Failure(exitCode: Int, output: String) extends RunResult
    case object Cancelled extends RunResult
  }

  /** Run KSP for one project. Synchronous, blocking. The caller wraps this in `IO.blocking` and races against the kill signal.
    *
    * @param ksp
    *   resolved config produced by [[bleep.SymbolProcessorResolver]]
    * @param javaBin
    *   path to `java` (from `started.jvmCommand`)
    * @param cancellation
    *   token watched by the parent compile DAG; if cancelled, the child process is destroyed
    * @param logger
    *   surface to relay child process output
    * @return
    *   Success / Failure / Cancelled
    */
  def run(
      ksp: SymbolProcessorResult,
      decision: KspIncrementalState.Decision,
      javaBin: Path,
      maxMemory: Option[String],
      cancellation: CancellationToken,
      logger: Logger
  ): RunResult =
    // Bail before forking if the build is already being cancelled (e.g. user hit Ctrl-C between the task being scheduled and starting). A KSP fork is
    // ~150MB resident — not worth spinning one up just to destroy it.
    if (cancellation.isCancelled) RunResult.Cancelled
    else {
      // Create output dirs eagerly. KSP will write into them; missing parents otherwise produce confusing IOException stack traces from inside Analysis API.
      List(ksp.kotlinOutputDir, ksp.javaOutputDir, ksp.classOutputDir, ksp.resourceOutputDir, ksp.cachesDir).foreach { d =>
        Files.createDirectories(d)
      }

      val cmdLine = {
        val cmd = List.newBuilder[String]
        cmd += javaBin.toString
        // Heap cap follows the user-level `bspServer.kspRunnerMaxMemory` setting (None = JVM default). KSP's bundled IntelliJ-platform code is memory-hungry
        // on real builds but tiny on toy fixtures, which is why we don't pick a default here — sized at the call site instead.
        maxMemory.foreach(m => cmd += s"-Xmx$m")
        // Suppress the runner's noisy `sun.misc.Unsafe::objectFieldOffset` deprecation warnings on JDK 21+. JetBrains' own launcher uses these flags for the
        // same reason.
        cmd += "--add-opens=java.base/java.util=ALL-UNNAMED"
        cmd += "--add-opens=java.base/java.lang=ALL-UNNAMED"
        cmd += "-cp"
        cmd += ksp.runnerClasspath.iterator.map(_.toString).mkString(java.io.File.pathSeparator)
        cmd += "com.google.devtools.ksp.cmdline.KSPJvmMain"
        ksp.runnerArgs(decision).foreach(cmd += _)
        cmd.result()
      }

      logger.debug(s"KSP cmd: ${cmdLine.mkString(" \\\n  ")}")
      logger.debug(s"KSP runner classpath (${ksp.runnerClasspath.size} jars): ${ksp.runnerClasspath.iterator.map(_.getFileName.toString).mkString(", ")}")
      logger.debug(s"KSP processor classpath (${ksp.processorJars.size} jars): ${ksp.processorJars.iterator.map(_.getFileName.toString).mkString(", ")}")
      logger.debug(s"KSP libraries (${ksp.librariesClasspath.size} jars): ${ksp.librariesClasspath.iterator.map(_.getFileName.toString).mkString(", ")}")

      val pb = new ProcessBuilder(cmdLine.asJava)
      pb.redirectErrorStream(true)
      val process = pb.start()

      // Drain output on a background thread so the OS pipe doesn't fill and block KSP. Cap accumulation by `MaxChars` UTF-16 code units (StringBuilder.length is
      // chars, not bytes). The cap is conservative; a runaway processor that logs hundreds of MB would otherwise tip bleep-bsp's heap.
      val output = new StringBuilder
      val MaxChars = 1024 * 1024
      val drainer = new Thread(
        () => {
          val reader = new java.io.BufferedReader(new java.io.InputStreamReader(process.getInputStream, "UTF-8"))
          try {
            var line = reader.readLine()
            while (line != null) {
              output.synchronized {
                if (output.length < MaxChars) {
                  output.append(line).append('\n')
                  if (output.length >= MaxChars) output.append("... [output truncated] ...\n")
                }
              }
              line = reader.readLine()
            }
          } catch {
            case _: java.io.IOException => () // pipe closed when the child exits; normal
          } finally
            try reader.close()
            catch { case _: Throwable => () }
        },
        "bleep-ksp-output-drain"
      )
      drainer.setDaemon(true)
      drainer.start()

      // Poll: if cancellation fires, destroy the child. Use 100ms checks rather than `process.waitFor()` so cancellation latency is bounded. The loop exits via
      // either `cancelled` (sets the flag and breaks) or process exit (terminates the while condition naturally).
      var cancelled = false
      while (!cancelled && process.isAlive)
        if (cancellation.isCancelled) {
          process.destroyForcibly()
          process.waitFor(5, java.util.concurrent.TimeUnit.SECONDS)
          drainer.join(1000)
          cancelled = true
        } else {
          try process.waitFor(100, java.util.concurrent.TimeUnit.MILLISECONDS)
          catch { case _: InterruptedException => () }
        }

      if (cancelled) RunResult.Cancelled
      else {
        drainer.join(2000)
        val captured = output.synchronized(output.toString)
        val exit = process.exitValue()
        if (exit == 0) {
          if (captured.nonEmpty) logger.debug(s"KSP output:\n$captured")
          RunResult.Success
        } else RunResult.Failure(exit, captured)
      }
    }
}
