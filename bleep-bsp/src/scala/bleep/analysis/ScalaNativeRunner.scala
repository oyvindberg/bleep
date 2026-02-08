package bleep.analysis

import bleep.bsp.Outcome.{KillReason, RunOutcome}
import bleep.bsp.{Outcome, ProcessRunner}
import cats.effect.{Deferred, IO}
import java.nio.file.Path
import scala.jdk.CollectionConverters.*

/** Runner for Scala Native binaries.
  *
  * Executes the linked native binary directly. All process management uses ProcessRunner for proper cleanup on kill.
  */
object ScalaNativeRunner {

  /** Run a Scala Native binary.
    *
    * @param binary
    *   the path to the native binary
    * @param args
    *   command-line arguments to pass to the program
    * @param workingDir
    *   the working directory for execution
    * @param env
    *   environment variables
    * @param killSignal
    *   Deferred that can be completed to kill the process
    * @return
    *   RunOutcome indicating what happened
    */
  def run(
      binary: Path,
      args: Seq[String],
      workingDir: Path,
      env: Map[String, String],
      killSignal: Deferred[IO, KillReason]
  ): IO[RunOutcome] = {
    val command = Seq(binary.toAbsolutePath.toString) ++ args
    val pb = new ProcessBuilder(command.asJava)
      .directory(workingDir.toFile)
    env.foreach { case (k, v) => pb.environment().put(k, v) }

    ProcessRunner.runWithOutput(pb, killSignal)
  }

  /** Run a Scala Native binary, inheriting IO.
    *
    * Use this when you want the process output to go directly to the terminal.
    */
  def runInheritIO(
      binary: Path,
      args: Seq[String],
      workingDir: Path,
      env: Map[String, String],
      killSignal: Deferred[IO, KillReason]
  ): IO[RunOutcome] = {
    val command = Seq(binary.toAbsolutePath.toString) ++ args
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

  /** Check if a binary is executable.
    *
    * @param binary
    *   the path to check
    * @return
    *   true if the binary exists and is executable
    */
  def isExecutable(binary: Path): Boolean =
    java.nio.file.Files.exists(binary) && java.nio.file.Files.isExecutable(binary)

  /** Make a binary executable (on Unix systems).
    *
    * @param binary
    *   the path to the binary
    * @return
    *   IO that makes the binary executable
    */
  def makeExecutable(binary: Path): IO[Unit] = IO.blocking {
    import java.nio.file.attribute.PosixFilePermission
    import java.nio.file.Files
    import scala.jdk.CollectionConverters.*

    if (!isWindows) {
      val perms = Files.getPosixFilePermissions(binary).asScala
      val newPerms = perms ++ Set(
        PosixFilePermission.OWNER_EXECUTE,
        PosixFilePermission.GROUP_EXECUTE,
        PosixFilePermission.OTHERS_EXECUTE
      )
      Files.setPosixFilePermissions(binary, newPerms.asJava)
    }
  }

  /** Detect the current operating system. */
  def isWindows: Boolean =
    System.getProperty("os.name").toLowerCase.contains("windows")

  def isMacOS: Boolean =
    System.getProperty("os.name").toLowerCase.contains("mac")

  def isLinux: Boolean =
    System.getProperty("os.name").toLowerCase.contains("linux")

  /** Get the expected binary file extension for the current platform. */
  def binaryExtension: String =
    if (isWindows) ".exe" else ""

  /** Get the expected dynamic library extension for the current platform. */
  def dynamicLibraryExtension: String =
    if (isWindows) ".dll"
    else if (isMacOS) ".dylib"
    else ".so"

  /** Get the expected static library extension for the current platform. */
  def staticLibraryExtension: String =
    if (isWindows) ".lib" else ".a"
}
