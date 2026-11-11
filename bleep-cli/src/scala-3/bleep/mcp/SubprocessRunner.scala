package bleep.mcp

import bleep.logs.{abbreviate, LogShape, StreamRecorder}
import cats.effect.{IO, Resource}

import java.nio.file.Path

/** How one subprocess ended. `exitCode` is absent exactly when `timedOut` — a killed process has no exit status worth reporting. Its output is on disk in the
  * run-log directory; what comes back here is the shape of it.
  */
case class RunOutcome(
    exitCode: Option[Int],
    timedOut: Boolean,
    durationMs: Long,
    pid: Long,
    stdout: StreamRecorder.Result,
    stderr: StreamRecorder.Result,
    timing: LogShape.Timing
)

/** Runs one forked program for `bleep.run`, recording its output into a run-log directory as it arrives and reporting progress while it runs.
  *
  * Separate from the MCP server because nothing here is about MCP: it takes callbacks rather than a `CallContext`, which is what lets it be tested against real
  * processes instead of only through a live BSP connection.
  */
object SubprocessRunner {

  val HeartbeatInterval: scala.concurrent.duration.FiniteDuration = {
    import scala.concurrent.duration.*
    1.second
  }

  val MaxHeartbeatLineChars = 160

  /** A runaway program stops being recorded at this many bytes per stream; the rest is counted as dropped. */
  val MaxStreamBytes: Long = 64L * 1024 * 1024

  /** Run `cmd` to completion or to the timeout, recording stdout and stderr into `logDir`. The program sees exactly `env`, not the MCP server's environment.
    *
    * A timeout does not raise: the output of a program that had to be killed is the output most worth reading. It comes back with `timedOut` set and no exit
    * code.
    *
    * @param onStarted
    *   called with the pid once the process exists
    * @param onProgress
    *   called about once a second with a status line; see [[statusLine]]
    */
  def run(
      cmd: List[String],
      cwd: Path,
      env: List[(String, String)],
      timeoutSeconds: Int,
      logDir: Path,
      maxStreamBytes: Long,
      onStarted: Long => IO[Unit],
      onProgress: String => IO[Unit]
  ): IO[RunOutcome] = {
    import scala.jdk.CollectionConverters.*

    for {
      startedAtMs <- IO(System.currentTimeMillis())
      silence = new LogShape.Silence(startedAtMs)
      outcome <- Resource
        .make(IO.blocking((new StreamRecorder("stdout", logDir, maxStreamBytes, silence), new StreamRecorder("stderr", logDir, maxStreamBytes, silence)))) {
          case (out, err) => IO.blocking { out.close(): Unit; err.close(): Unit }
        }
        .use { case (out, err) =>
          for {
            heartbeatFiber <- heartbeat(startedAtMs, out, err, onProgress).start
            outcome <- Resource
              .make(IO.blocking {
                val builder = new java.lang.ProcessBuilder(cmd.asJava)
                builder.directory(cwd.toFile)
                builder.environment().clear()
                env.foreach { case (k, v) => builder.environment().put(k, v) }
                builder.start()
              })(proc => IO.blocking { proc.destroyForcibly(); () }) // a no-op once it has exited; the point is that cancelling the call cannot leak a JVM
              .use { proc =>
                onStarted(proc.pid()) >> IO.interruptible {
                  // One reader thread per stream: a process that fills the OS pipe buffer blocks until someone drains it, and the heartbeat wants the lines
                  // as they arrive rather than when the process exits.
                  val stdoutThread = pump(proc.getInputStream, out)
                  val stderrThread = pump(proc.getErrorStream, err)
                  val pid = proc.pid()

                  if (proc.waitFor(timeoutSeconds.toLong, java.util.concurrent.TimeUnit.SECONDS)) {
                    stdoutThread.join(5000)
                    stderrThread.join(5000)
                    val endedAtMs = System.currentTimeMillis()
                    RunOutcome(Some(proc.exitValue()), timedOut = false, endedAtMs - startedAtMs, pid, out.close(), err.close(), silence.result(endedAtMs))
                  } else {
                    proc.destroyForcibly()
                    // The readers get a moment to drain what is still buffered: the output of a program we had to kill is the output we most need to see.
                    stdoutThread.join(1000)
                    stderrThread.join(1000)
                    val endedAtMs = System.currentTimeMillis()
                    RunOutcome(None, timedOut = true, endedAtMs - startedAtMs, pid, out.close(), err.close(), silence.result(endedAtMs))
                  }
                }
              }
              .guarantee(heartbeatFiber.cancel)
          } yield outcome
        }
    } yield outcome
  }

  /** One heartbeat line: how long the program has been going, how much it has said, and the last thing it said.
    *
    * The newest line is the part that earns its keep. The compile heartbeat can say how many projects are done because the build has that structure; a running
    * program has none, and elapsed time alone cannot distinguish one doing work from one deadlocked on a socket. The line it printed four seconds ago can.
    */
  def statusLine(elapsedSeconds: Long, stdoutLines: Int, stderrLines: Int, lastLine: Option[String]): String = {
    val parts = List.newBuilder[String]
    parts += s"${elapsedSeconds}s"
    if (stdoutLines > 0) parts += s"$stdoutLines lines stdout"
    if (stderrLines > 0) parts += s"$stderrLines lines stderr"
    if (stdoutLines == 0 && stderrLines == 0) parts += "no output yet"
    lastLine.foreach(line => parts += s"last: ${abbreviate(line, MaxHeartbeatLineChars)}")
    parts.result().mkString(", ")
  }

  private def heartbeat(startedAtMs: Long, out: StreamRecorder, err: StreamRecorder, onProgress: String => IO[Unit]): IO[Unit] = {
    val tick =
      for {
        elapsedSeconds <- IO((System.currentTimeMillis() - startedAtMs) / 1000)
        // stderr wins the last-line slot: that is where a program in trouble talks
        _ <- onProgress(statusLine(elapsedSeconds, out.lineCount, err.lineCount, err.lastLine.orElse(out.lastLine)))
      } yield ()

    (IO.sleep(HeartbeatInterval) >> tick).foreverM.void
  }

  /** Drain one stream into its recorder, on its own daemon thread, until the stream ends or the recorder is closed. */
  private def pump(in: java.io.InputStream, recorder: StreamRecorder): Thread = {
    val thread = new Thread(() => {
      val buf = new Array[Byte](1 << 16)
      var n = in.read(buf)
      var keepGoing = true
      while (n >= 0 && keepGoing) {
        keepGoing = recorder.consume(buf, n, System.currentTimeMillis())
        if (keepGoing) n = in.read(buf)
      }
      if (n < 0) recorder.endOfInput(System.currentTimeMillis())
    })
    thread.setDaemon(true)
    thread.setName(s"bleep-run-${recorder.stream}")
    thread.start()
    thread
  }
}
