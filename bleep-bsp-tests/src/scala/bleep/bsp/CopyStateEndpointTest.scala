package bleep.bsp

import bleep.bsp.protocol.{BleepServerAdmin, CopyStateResponse}
import bleep.model
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import ryddig.{LogPatterns, Loggers}

import java.io.{InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicBoolean

/** bleep/copyState, driven the way a real client drives it: raw JSON-RPC with no BSP handshake, against real directories on the real filesystem.
  *
  * The filesystem part is the point: the clone transport differs per OS (clonefile on macOS, reflink on Linux, a JVM copy elsewhere), so this suite running in
  * CI on all three platforms is what proves each lane actually clones bytes. The lock test proves the other half of the contract — state is never copied out of
  * a project some client is compiling.
  */
class CopyStateEndpointTest extends AnyFunSuite with Matchers {

  /** An in-memory pipe with none of `java.io.Piped*`'s thread-liveness checks. Requests here are dispatched on background fibers, so responses arrive from
    * short-lived cats-effect threads — exactly the situation where PipedInputStream throws spurious "Pipe broken" because the last writer thread has exited.
    */
  private final class InMemoryPipe {
    private val queue = new LinkedBlockingQueue[Integer]()
    private val Eof = Integer.valueOf(-1)
    val out: OutputStream = new OutputStream {
      override def write(b: Int): Unit = queue.put(b & 0xff)
      override def close(): Unit = queue.put(Eof)
    }
    val in: InputStream = new InputStream {
      override def read(): Int = {
        val v = queue.poll(30, java.util.concurrent.TimeUnit.SECONDS)
        if (v == null) throw new java.io.IOException("test pipe: no data within 30s")
        else if (v.intValue() < 0) { queue.put(Eof); -1 }
        else v.intValue()
      }
      override def read(bytes: Array[Byte], off: Int, len: Int): Int =
        if (len == 0) 0
        else {
          val first = read()
          if (first < 0) -1
          else {
            bytes(off) = first.toByte
            var n = 1
            var next = queue.poll()
            while (n < len && next != null)
              if (next.intValue() < 0) { queue.put(Eof); next = null }
              else {
                bytes(off + n) = next.byteValue()
                n += 1
                next = if (n < len) queue.poll() else null
              }
            n
          }
        }
    }
  }

  private class Fixture {
    private val logger = Loggers.stderr(LogPatterns.logFile)
    private val toServer = new InMemoryPipe
    private val toClient = new InMemoryPipe
    private val clientToServer = toServer.out
    private val serverInput = toServer.in
    private val serverToClient = toClient.out
    private val clientInput = toClient.in

    private val analysisCache = new bleep.analysis.AnalysisCache

    val daemonInfo: DaemonInfo = DaemonInfo(
      startedAtEpochMs = 1_700_000_000_000L,
      pid = 4242L,
      socketDir = Files.createTempDirectory("copy-state-socket"),
      bleepVersion = "1.2.3-test",
      bootedConfig = bleep.model.BspServerConfig.default,
      connectionRegistry = new ConnectionRegistry(() => System.currentTimeMillis()),
      requestDaemonShutdown = () => new AtomicBoolean(true).set(true)
    )

    private val server = new MultiWorkspaceBspServer(
      serverInput,
      serverToClient,
      logger,
      machine = bleep.MachineResources.forThisMachine(totalCpu = 4, logger = logger),
      heapMonitor = HeapMonitor.system,
      kspMutexes = new KspMutexes,
      buildCache = new BuildCache(4, analysisCache),
      analysisCache = analysisCache,
      daemonInfo = daemonInfo,
      connId = 17
    )

    private val thread = new Thread(() => server.run(), "copy-state-endpoint-test-server")
    thread.setDaemon(true)
    thread.start()

    def shutdown(): Unit = {
      clientToServer.close()
      thread.join(10000)
    }

    def request(params: String): io.circe.Json = {
      val body = s"""{"jsonrpc":"2.0","id":1,"method":"${BleepServerAdmin.CopyStateMethod}","params":$params}"""
      val bytes = body.getBytes(StandardCharsets.UTF_8)
      clientToServer.write(s"Content-Length: ${bytes.length}\r\n\r\n".getBytes(StandardCharsets.UTF_8))
      clientToServer.write(bytes)
      clientToServer.flush()
      readMessage()
    }

    def copyState(from: Path, to: Path): io.circe.Json =
      request(s"""{"from":${quote(from)},"to":${quote(to)},"variant":"normal"}""")

    private def quote(p: Path): String = io.circe.Json.fromString(p.toString).noSpaces

    private def readMessage(): io.circe.Json = {
      // Content-Length counts BYTES; reading that many chars through a Reader hangs forever on any multi-byte character in the response
      def readLine(): String = {
        val sb = new StringBuilder
        var b = clientInput.read()
        while (b != -1 && b != '\n') {
          if (b != '\r') sb.append(b.toChar)
          b = clientInput.read()
        }
        sb.toString
      }

      var contentLength = -1
      var line = readLine()
      while (line.nonEmpty) {
        if (line.startsWith("Content-Length:")) contentLength = line.drop("Content-Length:".length).trim.toInt
        line = readLine()
      }
      if (contentLength < 0) fail("no Content-Length in response")

      val buf = new Array[Byte](contentLength)
      var read = 0
      while (read < contentLength) {
        val n = clientInput.read(buf, read, contentLength - read)
        if (n < 0) fail("stream closed mid-response")
        read += n
      }
      io.circe.parser.parse(new String(buf, 0, contentLength, StandardCharsets.UTF_8)).fold(err => fail(s"malformed response: $err"), identity)
    }
  }

  /** A workspace on disk with compiled-looking state for the given projects, without any build ever having run. */
  private def workspace(projects: String*): Path = {
    val dir = Files.createTempDirectory("copy-state-ws").toRealPath()
    Files.writeString(dir.resolve("bleep.yaml"), "$schema: fake\n")
    projects.foreach { name =>
      val crossName = model.CrossProjectName.fromString(name).getOrElse(fail(s"bad cross name in test: $name"))
      val paths = bleep.BuildPaths(dir, bleep.BuildLoader.find(dir), model.BuildVariant.Normal)
      val classes = paths.variantBuildDir(crossName).resolve("classes")
      Files.createDirectories(classes.resolve("com/example"))
      Files.write(classes.resolve("com/example/Main.class"), s"classbytes-$name".getBytes(StandardCharsets.UTF_8))
      val analysis = paths.zincAnalysisFile(crossName)
      Files.createDirectories(analysis.getParent)
      Files.write(analysis, s"analysis-$name".getBytes(StandardCharsets.UTF_8))
      Files.write(analysis.getParent.resolve("noop-manifest.bin"), s"manifest-$name".getBytes(StandardCharsets.UTF_8))
      Files.createDirectories(analysis.getParent.resolve("cache"))
      Files.write(analysis.getParent.resolve("cache/junk.bin"), s"zinc-cache-$name".getBytes(StandardCharsets.UTF_8))
      val generated = paths.generatedSourcesBaseDir(crossName).resolve("scripts")
      Files.createDirectories(generated)
      Files.write(generated.resolve("Gen.scala"), s"generated-$name".getBytes(StandardCharsets.UTF_8))
    }
    // workspace-level state: request transcripts describe THIS workspace's runs and must never seed a fork
    val requests = bleep.BuildPaths(dir, bleep.BuildLoader.find(dir), model.BuildVariant.Normal).requestsDir
    Files.createDirectories(requests)
    Files.write(requests.resolve("1.json"), "{}".getBytes(StandardCharsets.UTF_8))
    dir
  }

  private def freshWorkspace(): Path = {
    val dir = Files.createTempDirectory("copy-state-fresh").toRealPath()
    Files.writeString(dir.resolve("bleep.yaml"), "$schema: fake\n")
    dir
  }

  private def withServer(body: Fixture => Unit): Unit = {
    val f = new Fixture
    try body(f)
    finally f.shutdown()
  }

  private def result(json: io.circe.Json): CopyStateResponse =
    json.hcursor.downField("result").as[CopyStateResponse].fold(err => fail(s"expected a result, got: ${json.noSpaces} ($err)"), identity)

  private def errorMessage(json: io.circe.Json): String =
    json.hcursor.downField("error").downField("message").as[String].getOrElse(fail(s"expected an error, got: ${json.noSpaces}"))

  test("clones classes, analysis and generated sources; never the noop manifest or the lock file") {
    withServer { f =>
      val from = workspace("proj-a", "proj-b", "nested/main")
      val to = freshWorkspace()

      val response = result(f.copyState(from, to))
      response.projects shouldBe List("nested/main", "proj-a", "proj-b")

      val toPaths = bleep.BuildPaths(to, bleep.BuildLoader.find(to), model.BuildVariant.Normal)
      List("proj-a", "proj-b", "nested/main").foreach { name =>
        val crossName = model.CrossProjectName.fromString(name).get
        val classFile = toPaths.variantBuildDir(crossName).resolve("classes/com/example/Main.class")
        withClue(s"$name classes must be cloned byte-for-byte: ") {
          Files.readAllBytes(classFile) shouldBe s"classbytes-$name".getBytes(StandardCharsets.UTF_8)
        }
        Files.readAllBytes(toPaths.zincAnalysisFile(crossName)) shouldBe s"analysis-$name".getBytes(StandardCharsets.UTF_8)
        Files.readAllBytes(toPaths.generatedSourcesBaseDir(crossName).resolve("scripts/Gen.scala")) shouldBe s"generated-$name".getBytes(StandardCharsets.UTF_8)

        withClue("a copied noop manifest validates against the SOURCE workspace's absolute paths — a false noop pointing at the parent's classes: ") {
          Files.exists(toPaths.zincDir(crossName).resolve("noop-manifest.bin")) shouldBe false
        }
        withClue("a lock file must never be inherited: ") {
          Files.exists(toPaths.variantBuildDir(crossName).resolve(".bleep-lock")) shouldBe false
        }
        withClue("zinc's cache dir is per-workspace state and regenerates — sharing it is decided by StateSharing's allow-list, not a local deny-list: ") {
          Files.exists(toPaths.zincDir(crossName).resolve("cache")) shouldBe false
        }
      }
      withClue("request transcripts describe the SOURCE workspace's runs; a fork claiming them would be lying: ") {
        Files.exists(toPaths.requestsDir) shouldBe false
      }
    }
  }

  test("waits for an in-flight compile of a source project instead of copying mid-write") {
    withServer { f =>
      val from = workspace("proj-a")
      val to = freshWorkspace()
      val fromPaths = bleep.BuildPaths(from, bleep.BuildLoader.find(from), model.BuildVariant.Normal)
      val crossName = model.CrossProjectName.fromString("proj-a").get

      // pose as a compile: hold the exclusive lock copy-state's shared acquisition must wait for
      val (_, release) = ProjectLock
        .acquire(
          project = crossName,
          outputDir = fromPaths.variantBuildDir(crossName).resolve("classes"),
          mode = ProjectLock.LockMode.Exclusive,
          timeout = scala.concurrent.duration.FiniteDuration(5, "seconds"),
          onContention = () => ()
        )
        .allocated
        .unsafeRunSync()

      val done = new AtomicBoolean(false)
      val copyThread = new Thread(() => {
        result(f.copyState(from, to))
        done.set(true)
      })
      copyThread.start()

      Thread.sleep(500)
      withClue("copy-state must be blocked while the exclusive lock is held: ") {
        done.get() shouldBe false
      }

      release.unsafeRunSync()
      copyThread.join(30000)
      withClue("copy-state must complete once the lock is released: ") {
        done.get() shouldBe true
      }
    }
  }

  test("refuses a target that already has state") {
    withServer { f =>
      val from = workspace("proj-a")
      val to = workspace("proj-b")

      errorMessage(f.copyState(from, to)) should include("already has state")
    }
  }

  test("refuses a source with nothing to copy") {
    withServer { f =>
      errorMessage(f.copyState(freshWorkspace(), freshWorkspace())) should include("no compiled state")
    }
  }

  test("refuses from == to") {
    withServer { f =>
      val ws = workspace("proj-a")
      errorMessage(f.copyState(ws, ws)) should include("same workspace")
    }
  }

  test("refuses a directory that is not a workspace root") {
    withServer { f =>
      val from = workspace("proj-a")
      val to = freshWorkspace()
      val inside = Files.createDirectories(from.resolve("sub/dir"))

      errorMessage(f.copyState(inside, to)) should include("not a workspace root")
    }
  }
}
