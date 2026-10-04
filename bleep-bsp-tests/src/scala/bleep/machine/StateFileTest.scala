package bleep.machine

import bleep.machine.StateFile.{forkEncoder, stateDecoder, stateEncoder}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicBoolean
import scala.jdk.StreamConverters.StreamHasToScala

/** `state.json` (design §6): the v1 schema to the letter, atomic replacement under a concurrent reader, liveness, and discovery across socket directories. */
class StateFileTest extends AnyFunSuite with Matchers {

  private def withTempDir[A](f: Path => A): A = {
    val dir = Files.createTempDirectory("bleep-state-file-test")
    try f(dir)
    finally {
      val walk = Files.walk(dir)
      try walk.toScala(List).sortBy(p => -p.getNameCount).foreach(p => Files.delete(p))
      finally walk.close()
    }
  }

  private val self = StateFile.selfIdentity("test")

  private def state(pid: Long, startedAt: Long, forks: List[StateFork]): StateJson =
    StateJson(
      version = 1,
      pid = pid,
      startedAtEpochMs = startedAt,
      bleepVersion = "1.0.0-M15",
      updatedAtEpochMs = 1759474812345L,
      requests = 2,
      cpuInUse = 6,
      wantsMore = true,
      shuttingDown = false,
      forks = forks,
      idleSinceEpochMs = None
    )

  private val designExample =
    """{
      |  "version": 1,
      |  "pid": 12345,
      |  "startedAtEpochMs": 1759474800000,
      |  "bleepVersion": "1.0.0-M15",
      |  "updatedAtEpochMs": 1759474812345,
      |  "requests": 2,
      |  "cpuInUse": 6,
      |  "wantsMore": true,
      |  "shuttingDown": false,
      |  "forks": [
      |    { "id": 17, "pid": 23456, "kind": "test-batch", "boundMb": 3840, "state": "starting", "startedAtEpochMs": 1759474812000 },
      |    { "id": 12, "pid": 23401, "kind": "test-suite", "boundMb": 2560, "state": "measured", "footprintMb": 1310, "startedAtEpochMs": 1759474790000 }
      |  ]
      |}""".stripMargin

  private val designValue = state(
    pid = 12345L,
    startedAt = 1759474800000L,
    forks = List(
      StateFork(17L, Some(23456L), ForkKind.TestBatch, 3840L, StateForkState.Starting, 1759474812000L),
      StateFork(12L, Some(23401L), ForkKind.TestSuite, 2560L, StateForkState.Measured(1310L), 1759474790000L)
    )
  )

  test("the design's §6.2 example document decodes to the model") {
    io.circe.parser.decode[StateJson](designExample) shouldBe Right(designValue)
  }

  test("the model encodes to exactly the §6.2 fields, with footprintMb only on measured forks and pid only when known") {
    import io.circe.syntax._
    val json = designValue.asJson
    json.asObject.get.keys.toList shouldBe List(
      "version",
      "pid",
      "startedAtEpochMs",
      "bleepVersion",
      "updatedAtEpochMs",
      "requests",
      "cpuInUse",
      "wantsMore",
      "shuttingDown",
      "forks"
    )
    val forks = json.hcursor.downField("forks").as[List[io.circe.Json]].toOption.get
    forks(0).asObject.get.keys.toList shouldBe List("id", "kind", "boundMb", "startedAtEpochMs", "pid", "state")
    forks(1).asObject.get.keys.toList shouldBe List("id", "kind", "boundMb", "startedAtEpochMs", "pid", "state", "footprintMb")
    val unspawned = StateFork(1L, None, ForkKind.Ksp, 100L, StateForkState.Starting, 5L).asJson
    unspawned.asObject.get.keys.toList should not contain "pid"
    io.circe.parser.decode[StateJson](json.noSpaces) shouldBe Right(designValue)
  }

  test("idleSinceEpochMs is written only when set, and read back; a v1 file without it reads as busy") {
    import io.circe.syntax._
    val idle = designValue.copy(requests = 0, cpuInUse = 0, forks = Nil, idleSinceEpochMs = Some(1_759_474_500_000L))
    idle.asJson.asObject.get.keys.toList.last shouldBe "idleSinceEpochMs"
    io.circe.parser.decode[StateJson](idle.asJson.noSpaces) shouldBe Right(idle)
    io.circe.parser.decode[StateJson](designExample).toOption.get.idleSinceEpochMs shouldBe None
  }

  test("an unknown version is read as its forks and cpuInUse; without those it fails") {
    val future = """{"version": 7, "pid": 1, "startedAtEpochMs": 2, "cpuInUse": 3, "forks": [], "somethingNew": {"a": 1}}"""
    val read = io.circe.parser.decode[StateJson](future).toOption.get
    read.version shouldBe 7
    read.cpuInUse shouldBe 3
    read.forks shouldBe empty
    read.bleepVersion shouldBe "unknown (state.json version 7)"
    io.circe.parser.decode[StateJson]("""{"version": 7, "pid": 1, "startedAtEpochMs": 2}""").isLeft shouldBe true
    io.circe.parser.decode[StateJson]("""{"pid": 1}""").isLeft shouldBe true
  }

  test("an unknown fork kind or state fails rather than being guessed") {
    val badKind = designExample.replace("\"test-batch\"", "\"quantum\"")
    io.circe.parser.decode[StateJson](badKind).isLeft shouldBe true
    val badState = designExample.replace("\"starting\"", "\"warming\"")
    io.circe.parser.decode[StateJson](badState).isLeft shouldBe true
  }

  test("write then read round-trips; an absent file is None; a malformed file throws") {
    withTempDir { dir =>
      StateFile.read(dir) shouldBe None
      StateFile.write(dir, designValue)
      StateFile.read(dir) shouldBe Some(designValue)
      Files.list(dir).toScala(List).map(_.getFileName.toString) shouldBe List("state.json") // no tmp left behind
      Files.writeString(StateFile.file(dir), "{ not json")
      an[IllegalStateException] should be thrownBy StateFile.read(dir)
    }
  }

  test("a reader racing a writer sees whole documents only") {
    withTempDir { dir =>
      val a = state(1L, 1L, Nil)
      val b = state(2L, 2L, (1 to 40).toList.map(i => StateFork(i.toLong, Some(i.toLong), ForkKind.TestSuite, 1024L, StateForkState.Measured(900L), 0L)))
      StateFile.write(dir, a)
      val stop = new AtomicBoolean(false)
      val failure = new java.util.concurrent.atomic.AtomicReference[Throwable](null)
      val writer = new Thread(() => {
        var i = 0
        while (!stop.get()) {
          StateFile.write(dir, if (i % 2 == 0) b else a)
          i += 1
        }
      })
      writer.start()
      var reads = 0
      val deadline = System.nanoTime() + 500_000_000L
      try
        while (System.nanoTime() < deadline) {
          val read = StateFile.read(dir)
          (read == Some(a) || read == Some(b)) shouldBe true
          reads += 1
        }
      catch { case t: Throwable => failure.set(t) }
      finally {
        stop.set(true)
        writer.join()
      }
      if (failure.get() != null) throw failure.get()
      reads should be > 10
    }
  }

  test("liveness: our own pid and start instant are live; a wrong start instant or a dead pid are not") {
    StateFile.isLive(state(self.pid, self.startedAtEpochMs, Nil)) shouldBe true
    StateFile.isLive(state(self.pid, self.startedAtEpochMs + 1L, Nil)) shouldBe false
    val deadPid = Iterator.from(2_000_000).find(p => !ProcessHandle.of(p.toLong).isPresent).get.toLong
    StateFile.isLive(state(deadPid, self.startedAtEpochMs, Nil)) shouldBe false
  }

  test("discovery: two writers in two socket directories; dead files and our own are left out") {
    withTempDir { root =>
      val socketRoot = root.resolve("socket")
      Files.createDirectories(socketRoot)
      val dirs = List("aaaa", "bbbb", "cccc", "dddd", "eeee").map(h => Files.createDirectories(socketRoot.resolve(h)))
      val live1 =
        state(self.pid, self.startedAtEpochMs, List(StateFork(1L, None, ForkKind.Link, 512L, StateForkState.Starting, 1L))).copy(bleepVersion = "writer-1")
      val live2 = live1.copy(bleepVersion = "writer-2", cpuInUse = 9)
      val dead = state(self.pid, self.startedAtEpochMs + 1L, Nil).copy(bleepVersion = "dead")
      // Two servers write their own files at the same time; neither touches the other's.
      val t1 = new Thread(() => StateFile.write(dirs(0), live1))
      val t2 = new Thread(() => StateFile.write(dirs(1), live2))
      t1.start(); t2.start(); t1.join(); t2.join()
      StateFile.write(dirs(2), dead)
      Files.writeString(dirs(3).resolve("server.json"), "{}") // a pre-scheduler server: no state.json at all
      // dirs(4): empty directory

      val me = ServerIdentity(pid = 999_999_999L, startedAtEpochMs = 0L, bleepVersion = "me")
      StateFile.discoverOthers(socketRoot, me).map(_.bleepVersion).sorted shouldBe List("writer-1", "writer-2")
      StateFile.discoverOthers(socketRoot, ServerIdentity(self.pid, self.startedAtEpochMs, "me")).map(_.bleepVersion) shouldBe Nil
      StateFile.discoverOthers(root.resolve("missing"), me) shouldBe Nil
    }
  }

  test("the tick's discovery caches the directory listing for its ttl and reads every state file fresh") {
    withTempDir { root =>
      val socketRoot = Files.createDirectories(root.resolve("socket"))
      val me = ServerIdentity(pid = 999_999_999L, startedAtEpochMs = 0L, bleepVersion = "me")
      val clock = new java.util.concurrent.atomic.AtomicLong(10_000L)
      val discovery = new ServerDiscovery(socketRoot, me, () => clock.get(), listingTtlMs = 1000L)
      val live = state(self.pid, self.startedAtEpochMs, Nil)
      val a = Files.createDirectories(socketRoot.resolve("aaaa"))
      StateFile.write(a, live.copy(bleepVersion = "a", cpuInUse = 1))
      discovery.others().map(s => (s.bleepVersion, s.cpuInUse)) shouldBe List(("a", 1))
      // A new directory is not seen until the listing expires; a changed file in a known directory is seen at once.
      val b = Files.createDirectories(socketRoot.resolve("bbbb"))
      StateFile.write(b, live.copy(bleepVersion = "b"))
      StateFile.write(a, live.copy(bleepVersion = "a", cpuInUse = 7))
      clock.set(10_999L)
      discovery.others().map(s => (s.bleepVersion, s.cpuInUse)) shouldBe List(("a", 7))
      clock.set(11_000L)
      discovery.others().map(_.bleepVersion).sorted shouldBe List("a", "b")
      // A pruned directory's file is simply absent on the next read.
      Files.delete(StateFile.file(b))
      Files.delete(b)
      discovery.others().map(_.bleepVersion) shouldBe List("a")
    }
  }

  test("a state file that cannot be parsed fails discovery loudly") {
    withTempDir { root =>
      val dir = Files.createDirectories(root.resolve("ffff"))
      Files.writeString(StateFile.file(dir), """{"version": 1, "pid": "not a number"}""")
      an[IllegalStateException] should be thrownBy StateFile.discoverOthers(root, self)
    }
  }
}
