package bleep.mcp

import cats.effect.{IO, Resource}
import io.circe.Json

import java.nio.file.Path
import java.util.concurrent.atomic.AtomicReference

/** Truncate for display, marking that something was cut. */
private[mcp] def abbreviate(s: String, maxChars: Int): String =
  if (s.length <= maxChars) s else s.take(maxChars - 1) + "\u2026"

/** What a subprocess has printed so far: the first `HeadLines` and the last `TailLines` lines, with the middle counted rather than kept.
  *
  * Bounded on purpose. The whole output used to go into the tool result verbatim, so one chatty program could push everything else out of the agent's context.
  * Head and tail together answer both "how did it start" and "where did it get to", which is what an agent reading a run actually needs.
  */
case class StreamCapture(head: Vector[String], tail: Vector[String], totalLines: Int, omittedLines: Int) {
  def append(rawLine: String): StreamCapture = {
    val line = abbreviate(rawLine, StreamCapture.MaxLineChars)
    if (head.length < StreamCapture.HeadLines) copy(head = head :+ line, totalLines = totalLines + 1)
    else if (tail.length < StreamCapture.TailLines) copy(tail = tail :+ line, totalLines = totalLines + 1)
    else copy(tail = tail.tail :+ line, totalLines = totalLines + 1, omittedLines = omittedLines + 1)
  }

  def lastLine: Option[String] = tail.lastOption.orElse(head.lastOption)

  def render: String =
    if (omittedLines == 0) (head ++ tail).mkString("\n")
    else (head ++ Vector(s"... [$omittedLines lines omitted] ...") ++ tail).mkString("\n")

  /** Line counts alongside the text, so the agent can tell truncated output from short output instead of guessing. */
  def countsJson(prefix: String): List[(String, Json)] =
    List(s"${prefix}Lines" -> Json.fromInt(totalLines)) ++
      (if (omittedLines > 0) List(s"${prefix}OmittedLines" -> Json.fromInt(omittedLines)) else Nil)
}

object StreamCapture {
  val HeadLines = 200
  val TailLines = 200
  val MaxLineChars = 2000
  val empty: StreamCapture = StreamCapture(Vector.empty, Vector.empty, 0, 0)
}

/** How one subprocess ended. `exitCode` is absent exactly when `timedOut` — a killed process has no exit status worth reporting. */
case class RunOutcome(exitCode: Option[Int], timedOut: Boolean, durationMs: Long, pid: Long, stdout: StreamCapture, stderr: StreamCapture)

/** Runs one forked program for `bleep.run`, reporting progress while it runs and returning what it printed either way.
  *
  * Separate from the MCP server because nothing here is about MCP: it takes an `onProgress` callback rather than a `CallContext`, which is what lets it be
  * tested against real processes instead of only through a live BSP connection.
  */
object SubprocessRunner {

  val HeartbeatInterval: scala.concurrent.duration.FiniteDuration = {
    import scala.concurrent.duration.*
    1.second
  }

  val MaxHeartbeatLineChars = 160

  /** Run `cmd` to completion or to the timeout.
    *
    * A timeout used to raise, throwing away every byte the program had produced — the one case where that output is the whole point. It now comes back with
    * `timedOut` set and no exit code.
    *
    * @param onProgress
    *   called about once a second with a status line; see [[statusLine]].
    */
  def run(cmd: List[String], cwd: Path, timeoutSeconds: Int, onProgress: String => IO[Unit]): IO[RunOutcome] = {
    import scala.jdk.CollectionConverters.*

    for {
      stdoutRef <- IO(new AtomicReference(StreamCapture.empty))
      stderrRef <- IO(new AtomicReference(StreamCapture.empty))
      startedAtMs <- IO(System.currentTimeMillis())
      heartbeatFiber <- heartbeat(startedAtMs, stdoutRef, stderrRef, onProgress).start
      outcome <- Resource
        .make(IO.blocking {
          val builder = new java.lang.ProcessBuilder(cmd.asJava)
          builder.directory(cwd.toFile)
          builder.start()
        })(proc => IO.blocking { proc.destroyForcibly(); () }) // a no-op once it has exited; the point is that cancelling the tool call cannot leak a JVM
        .use { proc =>
          IO.interruptible {
            // One reader thread per stream: a process that fills the OS pipe buffer blocks until someone drains it, and the heartbeat wants the lines as they
            // arrive rather than when the process exits.
            val stdoutThread = pumpLines(proc.getInputStream, stdoutRef)
            val stderrThread = pumpLines(proc.getErrorStream, stderrRef)
            val pid = proc.pid()

            if (proc.waitFor(timeoutSeconds.toLong, java.util.concurrent.TimeUnit.SECONDS)) {
              stdoutThread.join(5000)
              stderrThread.join(5000)
              RunOutcome(Some(proc.exitValue()), timedOut = false, System.currentTimeMillis() - startedAtMs, pid, stdoutRef.get, stderrRef.get)
            } else {
              proc.destroyForcibly()
              // The readers get a moment to drain what is still buffered: the output of a program we had to kill is the output we most need to see.
              stdoutThread.join(1000)
              stderrThread.join(1000)
              RunOutcome(None, timedOut = true, System.currentTimeMillis() - startedAtMs, pid, stdoutRef.get, stderrRef.get)
            }
          }
        }
        .guarantee(heartbeatFiber.cancel)
    } yield outcome
  }

  /** One heartbeat line: how long the program has been going, how much it has said, and the last thing it said.
    *
    * The newest line is the part that earns its keep. The compile heartbeat can say how many projects are done because the build has that structure; a running
    * program has none, and elapsed time alone cannot distinguish one doing work from one deadlocked on a socket. The line it printed four seconds ago can.
    */
  def statusLine(elapsedSeconds: Long, stdout: StreamCapture, stderr: StreamCapture): String = {
    val parts = List.newBuilder[String]
    parts += s"${elapsedSeconds}s"
    if (stdout.totalLines > 0) parts += s"${stdout.totalLines} lines stdout"
    if (stderr.totalLines > 0) parts += s"${stderr.totalLines} lines stderr"
    if (stdout.totalLines == 0 && stderr.totalLines == 0) parts += "no output yet"
    stderr.lastLine.orElse(stdout.lastLine).foreach(line => parts += s"last: ${abbreviate(line, MaxHeartbeatLineChars)}")
    parts.result().mkString(", ")
  }

  private def heartbeat(
      startedAtMs: Long,
      stdoutRef: AtomicReference[StreamCapture],
      stderrRef: AtomicReference[StreamCapture],
      onProgress: String => IO[Unit]
  ): IO[Unit] = {
    val tick =
      for {
        out <- IO(stdoutRef.get)
        err <- IO(stderrRef.get)
        elapsedSeconds <- IO((System.currentTimeMillis() - startedAtMs) / 1000)
        _ <- onProgress(statusLine(elapsedSeconds, out, err))
      } yield ()

    (IO.sleep(HeartbeatInterval) >> tick).foreverM.void
  }

  /** Drain one stream line by line into `sink`, on its own daemon thread. */
  private def pumpLines(in: java.io.InputStream, sink: AtomicReference[StreamCapture]): Thread = {
    val thread = new Thread(() => {
      val reader = new java.io.BufferedReader(new java.io.InputStreamReader(in, java.nio.charset.StandardCharsets.UTF_8))
      var line = reader.readLine()
      while (line != null) {
        sink.updateAndGet(_.append(stripAnsi(line)))
        line = reader.readLine()
      }
    })
    thread.setDaemon(true)
    thread.start()
    thread
  }
}
