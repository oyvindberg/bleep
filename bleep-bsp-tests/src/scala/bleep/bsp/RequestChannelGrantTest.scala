package bleep.bsp

import bleep.analysis.TestScheduling
import bleep.machine.*
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.*

/** A grant the channel cannot place is given back, with the cpu the demand asked for — never dropped, never reported as an exit for a fork that lives on. */
class RequestChannelGrantTest extends AnyFunSuite with Matchers {
  private val r = RequestId("r")
  private val forkKey = ForkKey("k:exclusive")
  private def demand(task: String, cpu: Int) = ForkDemand(r, TaskId(task), ForkKind.TestSuite, forkKey, 1000L, cpu, shared = false)

  private def channel(scheduler: RecordingScheduler): RequestChannel = {
    val forks = new ForkRegistry
    new RequestChannel(
      r,
      RequestKind.Test,
      scheduler,
      forks,
      new ChildWatch(forks, ryddig.TypedLogger.DevNull),
      TestScheduling.noHeapWaits,
      () => HeapUsage(0L, 1024L),
      ryddig.TypedLogger.DevNull
    )
  }

  test("a fork grant for a task nothing waits for — not a handler, not the DAG — goes back: a reuse with its cpu, a spawn as exited") {
    val s = new RecordingScheduler
    val ch = channel(s)
    ch.setDagReady(Nil, Map.empty)
    ch.granted(TaskId("suite#7"), cpu = 2, Grant.Fork(ForkGrant.Reuse(ForkId(11L))), guaranteed = true)
    ch.granted(TaskId("suite#8"), cpu = 1, Grant.Fork(ForkGrant.Spawn(ForkId(12L))), guaranteed = false)
    s.workFinished.asScala.toList shouldBe List((ForkId(11L), 2))
    s.exited.asScala.toList shouldBe List(ForkId(12L))
    ch.takeGrants() shouldBe Nil
  }

  test("a grant for a DAG demand on the table is queued for the executor, with the demand's cpu") {
    val s = new RecordingScheduler
    val ch = channel(s)
    val d = InHeap(r, TaskId("compile:a"), InHeapKind.Compile, cpu = 1)
    ch.setDagReady(List(d), Map.empty)
    ch.granted(d.taskId, d.cpu, Grant.InHeap, guaranteed = true)
    ch.takeGrants() shouldBe List(Granted(d.taskId, 1, Grant.InHeap, guaranteed = true))
    s.inHeapFinished.asScala.toList shouldBe Nil
  }

  test("a grant for a handler's wait completes it; one arriving after the request ended goes back") {
    val s = new RecordingScheduler
    val ch = channel(s)
    val d = demand("suite#1", cpu = 1)
    val fiber = ch.acquire(d, "p").start.unsafeRunSync()
    SchedulerFakesEventually.eventually(2000L)(!s.submitted.isEmpty) shouldBe true
    ch.granted(d.taskId, d.cpu, Grant.Fork(ForkGrant.Spawn(ForkId(1L))), guaranteed = true)
    fiber.joinWithNever.unsafeRunSync() shouldBe ForkGrant.Spawn(ForkId(1L))

    ch.close()
    ch.granted(TaskId("suite#2"), cpu = 3, Grant.Fork(ForkGrant.Reuse(ForkId(1L))), guaranteed = true)
    s.workFinished.asScala.toList shouldBe List((ForkId(1L), 3))
  }
}

private object SchedulerFakesEventually {
  def eventually(timeoutMs: Long)(condition: => Boolean): Boolean = {
    val deadline = System.nanoTime() + timeoutMs * 1_000_000L
    var ok = condition
    while (!ok && System.nanoTime() < deadline) { Thread.sleep(10L); ok = condition }
    ok
  }
}
