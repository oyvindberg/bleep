package bleep.bsp

import bleep.bsp.protocol.{BleepServerAdmin, DaemonStatus}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import ryddig.{LogPatterns, Loggers}

import java.io.{BufferedReader, InputStreamReader, PipedInputStream, PipedOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import java.util.concurrent.atomic.AtomicBoolean

/** The admin endpoints, driven the way a real client drives them: raw JSON-RPC over a socket, with no BSP handshake at all.
  *
  * That absence is the property under test. `bleep server ls` runs from any directory, including one with no bleep.yaml, so an observer has to be able to
  * connect, ask and leave without shipping a build or pretending to be a build client. Every other method on this server is gated behind `build/initialize`,
  * and it would be very easy to gate these by accident too — the failure would look like "ls does not work outside a project", which is exactly the case nobody
  * runs by hand.
  */
class BleepStatusEndpointTest extends AnyFunSuite with Matchers {

  private class Fixture {
    val shutdownRequested = new AtomicBoolean(false)
    val registry = new ConnectionRegistry(() => System.currentTimeMillis())

    private val logger = Loggers.stderr(LogPatterns.logFile)
    private val clientToServer = new PipedOutputStream()
    private val serverInput = new PipedInputStream(clientToServer, 65536)
    private val serverToClient = new PipedOutputStream()
    private val clientInput = new PipedInputStream(serverToClient, 65536)
    private val reader = new BufferedReader(new InputStreamReader(clientInput, StandardCharsets.UTF_8))

    private val analysisCache = new bleep.analysis.AnalysisCache

    val daemonInfo: DaemonInfo = DaemonInfo(
      startedAtEpochMs = 1_700_000_000_000L,
      pid = 4242L,
      socketDir = Paths.get("/tmp/sockets/deadbeef"),
      bleepVersion = "1.2.3-test",
      bootedConfig = bleep.model.BspServerConfig.default.copy(parallelism = Some(7), maxCachedWorkspaces = Some(9)),
      connectionRegistry = registry,
      requestDaemonShutdown = () => shutdownRequested.set(true)
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

    private val thread = new Thread(() => server.run(), "status-endpoint-test-server")
    thread.setDaemon(true)
    thread.start()

    def request(method: String, params: String): io.circe.Json = {
      val body = s"""{"jsonrpc":"2.0","id":1,"method":"$method","params":$params}"""
      val bytes = body.getBytes(StandardCharsets.UTF_8)
      clientToServer.write(s"Content-Length: ${bytes.length}\r\n\r\n".getBytes(StandardCharsets.UTF_8))
      clientToServer.write(bytes)
      clientToServer.flush()
      readMessage()
    }

    private def readMessage(): io.circe.Json = {
      var contentLength = -1
      var line = reader.readLine()
      while (line != null && line.nonEmpty) {
        if (line.startsWith("Content-Length:")) contentLength = line.drop("Content-Length:".length).trim.toInt
        line = reader.readLine()
      }
      if (contentLength < 0) fail("no Content-Length in response")

      val buf = new Array[Char](contentLength)
      var read = 0
      while (read < contentLength) {
        val n = reader.read(buf, read, contentLength - read)
        if (n < 0) fail("stream closed mid-response")
        read += n
        io.circe.parser.parse(new String(buf, 0, read)) match {
          case Right(json) => return json
          case Left(_)     => ()
        }
      }
      io.circe.parser.parse(new String(buf, 0, read)).fold(err => fail(s"malformed response: $err"), identity)
    }

    def status(observer: Boolean): DaemonStatus =
      request(BleepServerAdmin.StatusMethod, s"""{"observer":$observer}""").hcursor
        .downField("result")
        .as[DaemonStatus]
        .fold(err => fail(s"could not decode status: $err"), identity)
  }

  test("bleep/status answers without build/initialize, which is what lets ls run outside a project") {
    val f = new Fixture
    val status = f.status(observer = true)

    status.adminProtocolVersion shouldBe BleepServerAdmin.ProtocolVersion
    status.bleepVersion shouldBe "1.2.3-test"
    status.pid shouldBe 4242L
    status.startedAtEpochMs shouldBe 1_700_000_000_000L
    status.socketDir shouldBe "/tmp/sockets/deadbeef"
  }

  test("it reports the config the daemon booted with, not what is on disk now") {
    val f = new Fixture
    val config = f.status(observer = true).config

    config.parallelism shouldBe 7
    config.maxCachedWorkspaces shouldBe 9
  }

  test("the JVM numbers are measured, not placeholders") {
    val f = new Fixture
    val jvm = f.status(observer = true).jvm

    jvm.heapUsedMb should be > 0L
    jvm.heapMaxMb should be > 0L
    jvm.threads should be > 0
    jvm.gc should not be empty
  }

  test("the governor's view comes through, including its capacity") {
    val f = new Fixture
    val machine = f.status(observer = true).machine

    machine.totalCpu shouldBe 4
    machine.activeCompiles shouldBe 0
    withClue("an idle server has nothing running and nothing queued: ") {
      machine.active shouldBe empty
      machine.waiting shouldBe empty
    }
  }

  test("an observer is recorded as one, so it never holds the daemon open") {
    val f = new Fixture
    f.registry.register(17, System.currentTimeMillis())

    f.status(observer = true)

    withClue("the connection asking must be marked observer by the handler itself: ") {
      f.registry.nonObserverCount shouldBe 0
    }
    f.status(observer = true).connections.find(_.connId == 17).map(_.observer) shouldBe Some(true)
  }

  test("observer: false leaves the connection counting as real use") {
    val f = new Fixture
    f.registry.register(17, System.currentTimeMillis())

    f.status(observer = false)

    f.registry.nonObserverCount shouldBe 1
  }

  test("bleep/shutdown asks the daemon to stop, distinct from build/shutdown ending one connection") {
    val f = new Fixture
    f.shutdownRequested.get() shouldBe false

    f.request(BleepServerAdmin.ShutdownMethod, "{}").hcursor.downField("result").focus shouldBe defined

    withClue("the daemon-wide shutdown callback must have fired: ") {
      f.shutdownRequested.get() shouldBe true
    }
  }

  test("an unknown bleep method is still refused, so the exemption is a list and not a prefix") {
    val f = new Fixture
    val response = f.request("bleep/somethingElse", "{}")

    response.hcursor.downField("error").focus shouldBe defined
  }
}
