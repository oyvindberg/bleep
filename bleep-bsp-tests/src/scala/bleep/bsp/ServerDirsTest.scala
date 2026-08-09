package bleep.bsp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.channels.ServerSocketChannel
import java.nio.file.{Files, Path}

/** `bleep server ls` filters nothing, and these are the states it must keep telling the truth about.
  *
  * The distinctions are not cosmetic. "That pid is gone" and "the server stopped" disagree on a real machine — a socket directory can hold a pid file naming a
  * long-dead process while a live daemon still serves the socket — and a `kill` built on the wrong one reports success it did not achieve. So liveness is a
  * connect probe here, always, and the pid file is only ever used to explain what it found.
  */
class ServerDirsTest extends AnyFunSuite with Matchers {

  private def tempDir(): Path = {
    val d = Files.createTempDirectory("bleep-server-dirs")
    d.toFile.deleteOnExit()
    d
  }

  /** A real unix socket someone is listening on, which is the only thing that makes a directory "running". */
  private def withListeningSocket(socketDir: Path)(body: => Unit): Unit = {
    val address = java.net.UnixDomainSocketAddress.of(socketDir.resolve("socket"))
    val channel = ServerSocketChannel.open(java.net.StandardProtocolFamily.UNIX)
    try {
      channel.bind(address): Unit
      body
    } finally channel.close()
  }

  private def writePid(socketDir: Path, pid: Long): Unit =
    Files.writeString(socketDir.resolve("pid"), pid.toString): Unit

  /** A pid that is definitely not a live process. */
  private def deadPid(): Long = {
    val process = new ProcessBuilder("true").start()
    process.waitFor(): Unit
    process.pid()
  }

  test("a socket someone is listening on is running, whatever the pid file says") {
    val dir = tempDir()
    writePid(dir, deadPid())
    withListeningSocket(dir) {
      // The pid is dead and the socket is live. Trusting the pid file here is what made `kill` claim success while the daemon kept serving.
      ServerDirs.classify(dir).state shouldBe ServerState.Running
    }
  }

  test("a live process whose socket refuses is wedged — the row kill exists for") {
    val dir = tempDir()
    writePid(dir, ProcessHandle.current().pid())
    ServerDirs.classify(dir).state shouldBe ServerState.Wedged
  }

  test("a pid file naming a dead process is dead") {
    val dir = tempDir()
    writePid(dir, deadPid())
    ServerDirs.classify(dir).state shouldBe ServerState.Dead(crashed = false)
  }

  test("an OOM marker in a rotated log makes it dead (crashed)") {
    val dir = tempDir()
    writePid(dir, deadPid())
    Files.writeString(dir.resolve("output.1"), s"blah\n${BspServerOperations.OomMarker}\nblah"): Unit

    withClue("a crash usually leaves its evidence in a rotated generation, not the current one: ") {
      ServerDirs.classify(dir).state shouldBe ServerState.Dead(crashed = true)
    }
  }

  test("files with no pid and no socket are litter, still listed and still sized") {
    val dir = tempDir()
    Files.writeString(dir.resolve("output"), "x" * 4096): Unit

    val info = ServerDirs.classify(dir)
    info.state shouldBe ServerState.Litter
    info.sizeBytes should be >= 4096L
  }

  test("a directory with no server.json reports unknown rather than hiding") {
    val info = ServerDirs.classify(tempDir())
    info.identity shouldBe None
    info.bleepVersion should include("unknown")
  }

  test("server.json supplies identity for a daemon too dead to be asked") {
    val dir = tempDir()
    writePid(dir, deadPid())
    ServerJson.write(
      dir,
      ServerJson(
        bleepVersion = "1.0.0-M11",
        jvmName = "graalvm-community",
        jvmVersion = "25.0.1",
        javaBin = "/opt/jvm/bin/java",
        javaOpts = List("-Xmx8g"),
        serverMainClass = "bleep.bsp.BspServerDaemon",
        command = List("java", "bleep.bsp.BspServerDaemon"),
        workingDir = dir.toString,
        spawnedAtEpochMs = 1L
      )
    )

    val info = ServerDirs.classify(dir)
    info.bleepVersion shouldBe "1.0.0-M11"
    info.jvm shouldBe "graalvm-community:25.0.1"
  }

  // ── resolving an id ──────────────────────────────────────────────

  private def info(hash: String, pid: Option[Long]): ServerDirInfo =
    ServerDirInfo(
      socketDir = Path.of("/tmp").resolve(hash),
      hash = hash,
      state = ServerState.Litter,
      pid = pid,
      identity = None,
      sizeBytes = 0L
    )

  private val candidates = List(info("aaaa1111", Some(100L)), info("aaaa2222", Some(200L)), info("bbbb3333", None))

  test("an id resolves by full hash, by pid, and by unambiguous prefix") {
    ServerDirs.resolve(candidates, "aaaa1111").map(_.hash) shouldBe Right("aaaa1111")
    ServerDirs.resolve(candidates, "200").map(_.hash) shouldBe Right("aaaa2222")
    ServerDirs.resolve(candidates, "bb").map(_.hash) shouldBe Right("bbbb3333")
  }

  test("an ambiguous prefix names the candidates instead of picking one") {
    val error = ServerDirs.resolve(candidates, "aaaa").left.getOrElse(fail("expected ambiguity"))
    error should include("ambiguous")
    error should include("aaaa1111")
    error should include("aaaa2222")
  }

  // ── orphans ──────────────────────────────────────────────────────

  test("the socket directory is recovered from a daemon's command line") {
    val commandLine = "/opt/jvm/bin/java -Xmx12g -cp a.jar:b.jar bleep.bsp.BspServerDaemon --socket /tmp/sockets/abc123"
    ServerDirs.socketDirOf(commandLine) shouldBe Some("/tmp/sockets/abc123")
  }

  test("a command line with no --socket yields nothing rather than guessing") {
    ServerDirs.socketDirOf("/opt/jvm/bin/java -cp a.jar bleep.bsp.BspServerDaemon") shouldBe None
  }

  /** A force-stop deletes the socket directory after killing, so a process that survives the kill is orphaned by its own cleanup — and then invisible to a
    * listing built by scanning those directories, while it goes on holding its heap. One was found two days old holding 4.1GB that nothing could name.
    */
  test("scanning for orphans does not mistake a live, known server for one") {
    val known = ServerDirs.scan(bleep.UserPaths.fromAppDirs)
    val orphans = ServerDirs.orphanDaemons(known)

    withClue(s"orphans: $orphans, known: ${known.map(_.hash)}: ") {
      orphans.map(_.socketDir).toSet.intersect(known.map(_.socketDir.toString).toSet) shouldBe empty
    }
  }

  test("an id matching nothing says so and points at ls") {
    ServerDirs.resolve(candidates, "zzzz").left.getOrElse(fail("expected failure")) should include("bleep server ls")
  }
}
