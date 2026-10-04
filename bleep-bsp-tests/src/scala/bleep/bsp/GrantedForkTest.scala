package bleep.bsp

import bleep.machine.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path
import java.util.concurrent.{ConcurrentLinkedQueue, TimeUnit}
import scala.jdk.CollectionConverters.*

/** One grant, several processes in a row: each is reported to the scheduler as it starts, the registry points at the one that exists now, and its `kill` kills
  * that one.
  */
class GrantedForkTest extends AnyFunSuite with Matchers {

  /** Records what a fork handle tells the scheduler. */
  private final class RecordingScheduler extends MachineScheduler {
    val spawned = new ConcurrentLinkedQueue[(ForkId, Long)]()
    val exited = new ConcurrentLinkedQueue[ForkId]()
    def registerRequest(id: RequestId, kind: RequestKind): Unit = ()
    def unregisterRequest(id: RequestId): Unit = ()
    def submitReady(request: RequestId, ready: List[Demand], unstartedSuitesByKey: Map[ForkKey, Int]): Unit = ()
    def inHeapFinished(request: RequestId, taskId: TaskId): Unit = ()
    def forkSpawned(fork: ForkId, pid: Long): Unit = spawned.add((fork, pid)): Unit
    def forkWorkFinished(fork: ForkId, cpu: Int): Unit = ()
    def forkExited(fork: ForkId): Unit = exited.add(fork): Unit
  }

  private def sleeper(ms: Long): Process =
    new ProcessBuilder(
      Path.of(System.getProperty("java.home"), "bin", "java").toString,
      "-cp",
      System.getProperty("java.class.path"),
      "bleep.machine.SleepMain",
      ms.toString
    )
      .redirectErrorStream(true)
      .start()

  test("successive processes under one grant are each reported, the registry knows what is alive under it, and kill reaches it") {
    val scheduler = new RecordingScheduler
    val forks = new ForkRegistry
    val grant = new GrantedFork(ForkId(7L), "sourcegen:gen", ForkKey("sourcegen:gen"), scheduler, forks, new ChildWatch(forks, ryddig.TypedLogger.DevNull))
    val first = sleeper(20_000L)
    try {
      grant.started(first)
      scheduler.spawned.asScala.toList shouldBe List((ForkId(7L), first.pid()))
      forks.get(ForkId(7L)).map(_.pids()) shouldBe Some(Set(first.pid()))

      // The first script's JVM is done; the next starts under the same grant.
      first.destroyForcibly(): Unit
      first.waitFor(5, TimeUnit.SECONDS) shouldBe true
      val second = sleeper(20_000L)
      try {
        grant.started(second)
        scheduler.spawned.asScala.toList shouldBe List((ForkId(7L), first.pid()), (ForkId(7L), second.pid()))
        forks.live.map(_.pids()) shouldBe List(Set(second.pid())) // one entry; the first process is dead and no longer counted
        forks.size shouldBe 1

        // An eviction or a cancel kills what runs now.
        forks.get(ForkId(7L)).get.kill("bleep: test")
        second.waitFor(5, TimeUnit.SECONDS) shouldBe true
        second.isAlive shouldBe false
      } finally second.destroyForcibly(): Unit

      grant.ended()
      forks.size shouldBe 0
      // The executor, not the handle, tells the scheduler the fork exited: nothing here does.
      scheduler.exited.asScala.toList shouldBe Nil
    } finally first.destroyForcibly(): Unit
  }

  test("processes a toolchain started are observed by handle, once each however often they are seen, and kill reaches every one alive") {
    val scheduler = new RecordingScheduler
    val forks = new ForkRegistry
    val grant = new GrantedFork(ForkId(9L), "link:native", ForkKey("link:native"), scheduler, forks, new ChildWatch(forks, ryddig.TypedLogger.DevNull))
    val a = sleeper(20_000L)
    val b = sleeper(20_000L)
    try {
      grant.observed(a.toHandle)
      grant.observed(b.toHandle)
      grant.observed(a.toHandle) // a watcher scanning on a cadence sees the same process again
      scheduler.spawned.asScala.toList shouldBe List((ForkId(9L), a.pid()), (ForkId(9L), b.pid()))
      forks.get(ForkId(9L)).map(_.pids()) shouldBe Some(Set(a.pid(), b.pid()))
      forks.get(ForkId(9L)).get.kill("bleep: test")
      a.waitFor(5, TimeUnit.SECONDS) shouldBe true
      b.waitFor(5, TimeUnit.SECONDS) shouldBe true
      grant.livePids shouldBe Set.empty
    } finally {
      a.destroyForcibly(): Unit
      b.destroyForcibly(): Unit
    }
  }

  test("a process runner's start hook is the grant's reporter") {
    val scheduler = new RecordingScheduler
    val registry = new ForkRegistry
    val grant = new GrantedFork(ForkId(3L), "ksp:p", ForkKey("ksp:p"), scheduler, registry, new ChildWatch(registry, ryddig.TypedLogger.DevNull))
    val p = sleeper(0L)
    try {
      grant.onStarted(p)
      p.waitFor(10, TimeUnit.SECONDS) shouldBe true
      scheduler.spawned.asScala.toList shouldBe List((ForkId(3L), p.pid()))
    } finally p.destroyForcibly(): Unit
  }
}
