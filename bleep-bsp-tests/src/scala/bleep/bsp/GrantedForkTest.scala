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

  test("successive processes under one grant are each reported, the registry follows the current one, and kill reaches it") {
    val scheduler = new RecordingScheduler
    val forks = new ForkRegistry
    val grant = new GrantedFork(ForkId(7L), "sourcegen:gen", ForkKey("sourcegen:gen"), scheduler, forks)
    val first = sleeper(20_000L)
    try {
      grant.started(first)
      scheduler.spawned.asScala.toList shouldBe List((ForkId(7L), first.pid()))
      forks.get(ForkId(7L)).map(_.pid) shouldBe Some(first.pid())

      // The first script's JVM is done; the next starts under the same grant.
      first.destroyForcibly(): Unit
      first.waitFor(5, TimeUnit.SECONDS) shouldBe true
      val second = sleeper(20_000L)
      try {
        grant.started(second)
        scheduler.spawned.asScala.toList shouldBe List((ForkId(7L), first.pid()), (ForkId(7L), second.pid()))
        forks.live.map(_.pid) shouldBe List(second.pid()) // one entry, the current process
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

  test("a process runner's start hook is the grant's reporter") {
    val scheduler = new RecordingScheduler
    val grant = new GrantedFork(ForkId(3L), "ksp:p", ForkKey("ksp:p"), scheduler, new ForkRegistry)
    val p = sleeper(0L)
    try {
      grant.onStarted(p)
      p.waitFor(10, TimeUnit.SECONDS) shouldBe true
      scheduler.spawned.asScala.toList shouldBe List((ForkId(3L), p.pid()))
    } finally p.destroyForcibly(): Unit
  }
}
