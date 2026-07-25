package bleep.scripts.dev

import bleep.bsp.{BspRifle, BspRifleConfig, SetupBleepBsp}
import bleep.{BleepScript, Commands, Started}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import ryddig.LogLevel

import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** Adversarial stress harness for the BSP daemon lifecycle. NOT a test — a runnable main you drive by hand and tune against.
  *
  * It fans out N concurrent "clients", each looping `ensureRunningAndConnect` → `build/initialize` → read reply → disconnect against ONE shared throwaway
  * socket dir, while a chaos fiber `kill -9`s the daemon on an interval. That reproduces exactly the conditions that brick things in the field: a swarm of
  * clients racing to spawn the daemon, plus the daemon dying under live clients so the survivors must detect it and respawn — all without touching any real
  * daemon (its own temp socket dir).
  *
  * What it watches for:
  *   - BRICK: a client reports "failed to start within …" against a daemon that is actually up. This is the readiness bug
  *     ([[BspServerOperations.waitForServer]]) and must never happen — any occurrence fails the run.
  *   - respawn coverage: distinct daemon pids seen > 1, proving the kill/respawn path actually exercised.
  *   - churn failures (broken pipe / stream closed when the daemon is killed mid-use) are expected and only informational.
  *
  * Run: `bleep publish local-ivy` first (the harness forks the real bleep-bsp at `BleepVersion.current`), then `bleep bsp-stress [clients] [durationSec]
  * [killEveryMs] [connectTimeoutSec]` (positional, since bleep's script parser rejects `--`-flags). Defaults: 500 15s(30) 2000 20.
  */
object BspStress extends BleepScript("BspStress") {

  private final case class Args(clients: Int, durationSec: Int, killEveryMs: Int, connectTimeoutSec: Int)

  // Positional, because bleep's script arg parser rejects anything starting with `--`:
  //   bleep bsp-stress [clients] [durationSec] [killEveryMs] [connectTimeoutSec]
  private def parseArgs(args: List[String]): Args = {
    def at(i: Int, default: Int): Int = args.lift(i).flatMap(_.toIntOption).getOrElse(default)
    Args(
      clients = at(0, 500),
      durationSec = at(1, 30),
      killEveryMs = at(2, 2000),
      connectTimeoutSec = at(3, 20)
    )
  }

  /** Lock-free tallies shared across all client fibers. */
  private final class Stats {
    val attempts = new AtomicLong(0)
    val successes = new AtomicLong(0)
    val bricks = new AtomicLong(0) // "failed to start within" against a live daemon — THE bug
    val kills = new AtomicLong(0)
    val sumLatencyMs = new AtomicLong(0)
    val maxLatencyMs = new AtomicLong(0)
    val failuresByKind = new ConcurrentHashMap[String, AtomicLong]()
    val distinctPids = ConcurrentHashMap.newKeySet[Long]()
    // Peak number of spawned daemons ALIVE at the same instant. This is the fork-storm signal — distinctPids counts pid-file churn over the whole run (every
    // spawn attempt, winners and short-lived losers), which vastly overcounts; a healthy run keeps peakConcurrent at 1–2 even while distinctPids climbs.
    val peakConcurrent = new AtomicLong(0)

    def recordSuccess(latencyMs: Long): Unit = {
      successes.incrementAndGet(); sumLatencyMs.addAndGet(latencyMs)
      maxLatencyMs.accumulateAndGet(latencyMs, math.max): Unit
    }
    def recordFailure(err: Throwable): Unit = {
      val msg = Option(err.getMessage).getOrElse(err.getClass.getName)
      val kind =
        if (msg.contains("failed to start within")) { bricks.incrementAndGet(); "BRICK: failed-to-start-vs-live-daemon" }
        else if (msg.contains("Broken pipe") || msg.contains("reset") || msg.contains("closed") || msg.contains("EOF")) "churn: daemon killed mid-use"
        else if (msg.contains("connection lost") || msg.contains("connect")) "connect failure"
        else s"other: ${msg.take(60)}"
      failuresByKind.computeIfAbsent(kind, _ => new AtomicLong(0)).incrementAndGet(): Unit
    }
  }

  override def run(started: Started, commands: Commands, args: List[String]): Unit = {
    val a = parseArgs(args)
    val logger = started.logger
    // Per-client BSP lifecycle logging is deafening at N=thousands; keep only warnings/errors from the machinery itself.
    val quiet = logger.withMinLogLevel(LogLevel.warn)

    val baseConfig = SetupBleepBsp(
      compileServerMode = started.config.compileServerModeOrDefault,
      config = started.config,
      resolvedJvm = started.resolvedJvm.forceGet,
      userPaths = started.pre.userPaths,
      resolver = started.resolver,
      logger = logger,
      javaSemanticdbVersion = SetupBleepBsp.DefaultJavaSemanticdbVersion
    ).fold(
      e => throw new RuntimeException(s"Could not resolve the bleep-bsp server classpath — did you `bleep publish local-ivy`? ${e.getMessage}", e),
      identity
    )

    // Isolate to a throwaway socket dir so the swarm and the kills never touch the user's real daemons.
    val stressDir = Files.createTempDirectory("bleep-bsp-stress-")
    val config = baseConfig.copy(
      address = BspRifleConfig.Address.DomainSocket(stressDir.resolve("socket")),
      workingDir = stressDir
    )
    val pidFile = stressDir.resolve("pid")

    logger.info(s"BSP stress: ${a.clients} clients, ${a.durationSec}s, kill every ${a.killEveryMs}ms, socket dir $stressDir")

    val stats = new Stats

    def readPid(): Option[Long] =
      try if (Files.exists(pidFile)) Some(Files.readString(pidFile).trim.toLong) else None
      catch { case _: Throwable => None }

    def alive(pid: Long): Boolean =
      try new ProcessBuilder("kill", "-0", pid.toString).start().waitFor() == 0
      catch { case _: Throwable => false }

    /** One client attempt: connect (spawning the daemon if needed), send initialize, read one framed reply. */
    def oneAttempt: IO[Unit] = {
      val startNanos = new AtomicReference[Long](0L)
      IO(startNanos.set(System.nanoTime())) >>
        BspRifle
          .ensureRunningAndConnect(config, quiet)
          .use { conn =>
            IO.blocking {
              readPid().foreach(stats.distinctPids.add)
              val body =
                """{"jsonrpc":"2.0","id":1,"method":"build/initialize","params":{"displayName":"stress","version":"0","bspVersion":"2.1.0","rootUri":"file:///tmp","capabilities":{"languageIds":["scala"]}}}"""
                  .getBytes("UTF-8")
              conn.output.write(s"Content-Length: ${body.length}\r\n\r\n".getBytes("US-ASCII"))
              conn.output.write(body)
              conn.output.flush()
              readFramedReply(conn.input)
              // Hold the session briefly, as a real client does actual work between connect and disconnect. Without this the loop degenerates into a reconnect
              // firehose (thousands/s) that no single daemon's accept backlog survives — which masks, rather than tests, the spawn behaviour.
              Thread.sleep(75)
            }
          }
          .timeout((a.connectTimeoutSec + 30).seconds)
          .attempt
          .flatMap {
            case Right(_) => IO(stats.recordSuccess((System.nanoTime() - startNanos.get()) / 1000000L))
            case Left(e)  => IO(stats.recordFailure(e))
          }
          .guarantee(IO(stats.attempts.incrementAndGet()).void)
    }

    def clientLoop(deadlineMs: Long): IO[Unit] =
      IO.realTime.flatMap { now =>
        if (now.toMillis >= deadlineMs) IO.unit
        else oneAttempt >> IO.sleep(100.millis) >> clientLoop(deadlineMs)
      }

    // Track the peak count of simultaneously-alive spawned daemons — the honest fork-storm signal (see Stats.peakConcurrent).
    def sampler(deadlineMs: Long): IO[Unit] =
      IO.sleep(250.millis) >> IO {
        val alive = stats.distinctPids.asScala.count(pid => ProcessHandle.of(pid).isPresent)
        stats.peakConcurrent.accumulateAndGet(alive.toLong, math.max): Unit
      } >> IO.realTime.flatMap(now => if (now.toMillis >= deadlineMs) IO.unit else sampler(deadlineMs))

    def chaosLoop(deadlineMs: Long): IO[Unit] =
      IO.sleep(a.killEveryMs.millis) >> IO.realTime.flatMap { now =>
        if (now.toMillis >= deadlineMs) IO.unit
        else {
          val kill = IO
            .blocking {
              readPid() match {
                case Some(pid) if alive(pid) =>
                  new ProcessBuilder("kill", "-9", pid.toString).start().waitFor()
                  stats.kills.incrementAndGet(); ()
                case _ => ()
              }
            }
            .attempt
            .void
          kill >> chaosLoop(deadlineMs)
        }
      }

    val program = for {
      startMs <- IO.realTime.map(_.toMillis)
      deadlineMs = startMs + a.durationSec * 1000L
      clients = List.fill(a.clients)(clientLoop(deadlineMs))
      chaosFiber <- chaosLoop(deadlineMs).start
      samplerFiber <- sampler(deadlineMs).start
      _ <- clients.parSequence_
      _ <- chaosFiber.join.attempt
      _ <- samplerFiber.join.attempt
    } yield ()

    try program.unsafeRunSync()
    finally cleanup(stressDir, stats.distinctPids.asScala.toSet ++ readPid(), logger)

    report(stats, logger)
    if (stats.bricks.get() > 0 || stats.successes.get() == 0) {
      logger.error("BSP stress FAILED — see report above")
      sys.exit(1)
    }
  }

  /** Read exactly one `Content-Length`-framed JSON-RPC message, or throw on EOF/short read. */
  private def readFramedReply(in: java.io.InputStream): Unit = {
    val header = new StringBuilder
    var b = in.read()
    while (b != -1 && !header.endsWith("\r\n\r\n")) { header.append(b.toChar); b = in.read() }
    if (b == -1) throw new java.io.EOFException("stream closed before reply header")
    val len = header
      .toString()
      .linesIterator
      .collectFirst { case l if l.toLowerCase.startsWith("content-length:") => l.split(":", 2)(1).trim.toInt }
      .getOrElse(throw new RuntimeException("no Content-Length in reply"))
    var remaining = len
    val buf = new Array[Byte](8192)
    while (remaining > 0) {
      val n = in.read(buf, 0, math.min(buf.length, remaining))
      if (n == -1) throw new java.io.EOFException("stream closed mid-reply")
      remaining -= n
    }
  }

  private def cleanup(stressDir: Path, spawnedPids: Set[Long], logger: ryddig.Logger): Unit = {
    // Kill EVERY daemon this run ever spawned, not just the last pid file — the herd leaves losers retrying the lock for tens of seconds, and a chaos-killed
    // generation's replacements pile up. Then a pkill backstop catches any whose cmdline still points at our socket dir.
    spawnedPids.foreach { pid =>
      try new ProcessBuilder("kill", "-9", pid.toString).start().waitFor()
      catch { case _: Throwable => () }
    }
    try new ProcessBuilder("pkill", "-9", "-f", stressDir.getFileName.toString).start().waitFor()
    catch { case _: Throwable => () }
    logger.info(s"Killed ${spawnedPids.size} spawned daemon(s) + pkill backstop for ${stressDir.getFileName}")
    try Files.walk(stressDir).iterator().asScala.toList.reverse.foreach(p => Files.deleteIfExists(p): Unit)
    catch { case e: Throwable => logger.warn(s"Could not fully delete $stressDir: ${e.getMessage}") }
  }

  private def report(stats: Stats, logger: ryddig.Logger): Unit = {
    val att = stats.attempts.get()
    val ok = stats.successes.get()
    val meanMs = if (ok == 0) 0 else stats.sumLatencyMs.get() / ok
    logger.info("──────── BSP stress report ────────")
    logger.info(s"attempts=$att  successes=$ok (${if (att == 0) 0 else ok * 100 / att}%)  mean=${meanMs}ms  max=${stats.maxLatencyMs.get()}ms")
    logger.info(
      s"daemon kills=${stats.kills.get()}  PEAK concurrent daemons=${stats.peakConcurrent.get()} (want ~1-2: proves NO fork storm)  distinct pids seen=${stats.distinctPids
          .size()} (cumulative pid-file churn, not live count)"
    )
    if (stats.failuresByKind.isEmpty) logger.info("failures: none")
    else {
      logger.info("failures by kind:")
      stats.failuresByKind.asScala.toList.sortBy(-_._2.get()).foreach { case (k, n) => logger.info(f"  ${n.get()}%6d  $k") }
    }
    val bricks = stats.bricks.get()
    if (bricks > 0) logger.error(s"*** $bricks BRICK(s): a live daemon was reported 'failed to start' — the readiness bug reproduced ***")
    else logger.info("no bricks — readiness held under contention")
  }
}
