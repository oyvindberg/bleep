package bleep.analysis

import bleep.bsp.Outcome.{KillReason, RunOutcome}
import bleep.bsp.{Outcome, ProcessRunner}
import cats.effect.{Deferred, IO}
import java.nio.file.Path
import scala.jdk.CollectionConverters.*

/** Runner for Kotlin/JS JavaScript output.
  *
  * Executes the compiled JavaScript using a managed Node.js binary. All process management uses ProcessRunner for proper cleanup on kill.
  */
object KotlinJsRunner {

  /** Run a Kotlin/JS-generated JavaScript file with Node.js.
    *
    * @param jsFile
    *   the main JavaScript file to run
    * @param args
    *   command-line arguments to pass to the program
    * @param moduleKind
    *   the module kind (affects Node.js flags)
    * @param workingDir
    *   the working directory for execution
    * @param env
    *   environment variables
    * @param nodeBinary
    *   path to the managed node binary
    * @param killSignal
    *   Deferred that can be completed to kill the process
    * @return
    *   RunOutcome indicating what happened
    */
  def run(
      jsFile: Path,
      args: Seq[String],
      moduleKind: KotlinJsCompilerConfig.ModuleKind,
      workingDir: Path,
      env: Map[String, String],
      nodeBinary: String,
      killSignal: Deferred[IO, KillReason]
  ): IO[RunOutcome] = {
    val nodeArgs = moduleKind match {
      case KotlinJsCompilerConfig.ModuleKind.ESModule =>
        Seq("--experimental-vm-modules", jsFile.toAbsolutePath.toString) ++ args
      case _ =>
        Seq(jsFile.toAbsolutePath.toString) ++ args
    }

    val command = Seq(nodeBinary) ++ nodeArgs
    val pb = new ProcessBuilder(command.asJava)
      .directory(workingDir.toFile)
    env.foreach { case (k, v) => pb.environment().put(k, v) }

    ProcessRunner.runWithOutput(pb, killSignal)
  }

  /** Run a Kotlin/JS-generated JavaScript file with Node.js, inheriting IO.
    *
    * Use this when you want the process output to go directly to the terminal.
    */
  def runInheritIO(
      jsFile: Path,
      args: Seq[String],
      moduleKind: KotlinJsCompilerConfig.ModuleKind,
      workingDir: Path,
      env: Map[String, String],
      nodeBinary: String,
      killSignal: Deferred[IO, KillReason]
  ): IO[RunOutcome] = {
    val nodeArgs = moduleKind match {
      case KotlinJsCompilerConfig.ModuleKind.ESModule =>
        Seq("--experimental-vm-modules", jsFile.toAbsolutePath.toString) ++ args
      case _ =>
        Seq(jsFile.toAbsolutePath.toString) ++ args
    }

    val command = Seq(nodeBinary) ++ nodeArgs
    val pb = new ProcessBuilder(command.asJava)
      .directory(workingDir.toFile)
      .inheritIO()
    env.foreach { case (k, v) => pb.environment().put(k, v) }

    ProcessRunner.start(pb).use { process =>
      val work = IO.blocking(process.waitFor())
      Outcome.raceKill(killSignal)(work).map {
        case Left(exitCode) =>
          RunOutcome.fromExitCode(exitCode, "", "")
        case Right(reason) =>
          RunOutcome.Killed(reason, "", "")
      }
    }
  }
}
