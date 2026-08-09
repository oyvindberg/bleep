package bleep
package internal

import cats.effect.IO

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.file.{Files, Path}
import scala.concurrent.duration.*

/** How long a build operation may take before bleep cancels it and says why.
  *
  * @param duration
  *   wall-clock ceiling for the operation
  * @param dumpTo
  *   where the thread dump goes when the ceiling is hit. A file rather than the log, because the point is to survive the process exiting — in CI the
  *   interesting run is the one whose console output nobody kept.
  * @param jvmBinDirs
  *   JVM `bin` directories to find `jstack` in. The client is a native image with no JDK of its own, so without this it can only dump its own threads — see
  *   [[ChildProcessDiagnostics.dumpAll]].
  * @param serverPids
  *   compile servers to dump alongside this process. They are not descendants of the client, so nothing else finds them, and they are usually the half doing
  *   the work that got stuck.
  */
case class MaxTime(
    duration: FiniteDuration,
    dumpTo: Path,
    jvmBinDirs: List[Path],
    serverPids: List[Long]
)

object MaxTime {

  /** Run `program`, giving up after [[MaxTime.duration]].
    *
    * The ladder, in order:
    *
    *   1. cancel — cancelling the fiber cancels the outstanding BSP future, which makes lsp4j send `$/cancelRequest`. The server has that in its
    *      `immediatelyHandled` set, so it lands even while a compile is running in a background fiber.
    *   1. diagnose — dump this process's threads plus the compile server's to [[MaxTime.dumpTo]].
    *   1. fail — a [[BleepException]], so the command exits non-zero through the normal path.
    *
    * It deliberately stops there. Killing the compile server would be collateral for a daemon serving other workspaces, and buys nothing for the case this
    * exists for: the client exiting is already enough to end a CI step and let the telemetry steps run.
    *
    * `timeoutAndForget`, not `timeout`. `timeout` cancels the fiber and then WAITS for its finalizers to complete, so against a fiber wedged in uninterruptible
    * blocking — exactly what is worth bounding — it does not bound anything at all. Measured, not reasoned: swapping this one call to `timeout` makes
    * `MaxTimeTest`'s uncancellable case run to completion and return SUCCESS well past its deadline, rather than merely being late. `timeoutAndForget` abandons
    * the fiber and moves on. The abandoned fiber cannot delay exit either: `Main` calls `System.exit` unconditionally.
    */
  def bound[A](maxTime: Option[MaxTime], program: IO[A]): IO[A] =
    maxTime match {
      case None     => program
      case Some(mt) =>
        program.timeoutAndForget(mt.duration).handleErrorWith {
          case _: java.util.concurrent.TimeoutException =>
            IO.blocking(writeDump(mt)).flatMap(note => IO.raiseError(new BleepException.Text(message(mt, note))))
          case other => IO.raiseError(other)
        }
    }

  private val Syntax = """^(\d+)(s|m|h)$""".r

  /** Parse `90s`, `15m`, `1h`.
    *
    * Deliberately narrow. A bare number is rejected because `--max-time 30` reads as seconds to one person and minutes to the next, and a bound that silently
    * means something other than what was typed is worse than one that refuses to start.
    */
  def parse(str: String): Either[String, FiniteDuration] =
    str.trim match {
      case Syntax(amount, unit) =>
        val n = amount.toLong
        if (n <= 0) Left(s"--max-time must be greater than zero, got '$str'")
        else
          Right(unit match {
            case "s" => n.seconds
            case "m" => n.minutes
            case "h" => n.hours
            case _   => sys.error(s"unreachable unit '$unit'")
          })
      case other => Left(s"--max-time must look like 90s, 15m or 1h, got '$other'")
    }

  /** Rendered into the failure message rather than thrown.
    *
    * Not error-swallowing: both facts reach the user in one message. Letting a failure in the *diagnostics* replace the timeout that triggered them would
    * report the less useful of the two problems, and lose the one the user actually hit.
    */
  private def writeDump(mt: MaxTime): String =
    try {
      val bytes = new ByteArrayOutputStream()
      val out = new PrintStream(bytes, true, "UTF-8")
      ChildProcessDiagnostics.dumpAll(out, mt.jvmBinDirs, mt.serverPids)
      out.flush()
      Option(mt.dumpTo.getParent).foreach(Files.createDirectories(_))
      Files.write(mt.dumpTo, bytes.toByteArray)
      s"thread dump written to ${mt.dumpTo}"
    } catch {
      case th: Throwable => s"additionally, the thread dump could not be written to ${mt.dumpTo}: $th"
    }

  private def message(mt: MaxTime, dumpNote: String): String =
    s"Gave up after --max-time ${mt.duration}: cancelled the build and stopped waiting. $dumpNote"
}
