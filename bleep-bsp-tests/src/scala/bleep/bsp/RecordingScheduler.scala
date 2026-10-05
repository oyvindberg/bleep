package bleep.bsp

import bleep.machine.*

import java.util.concurrent.ConcurrentLinkedQueue

/** Records what the daemon's side tells the scheduler, for tests of the channel and the fork handle that need no ticks. */
final class RecordingScheduler extends MachineScheduler {
  val spawned = new ConcurrentLinkedQueue[(ForkId, Long)]()
  val exited = new ConcurrentLinkedQueue[ForkId]()
  val workFinished = new ConcurrentLinkedQueue[(ForkId, Int)]()
  val inHeapFinished = new ConcurrentLinkedQueue[(RequestId, TaskId)]()
  val submitted = new ConcurrentLinkedQueue[List[Demand]]()
  def registerRequest(id: RequestId, kind: RequestKind): Unit = ()
  def unregisterRequest(id: RequestId): Unit = ()
  def submitReady(request: RequestId, ready: List[Demand], unstartedSuitesByKey: Map[ForkKey, Int]): Unit = submitted.add(ready): Unit
  def inHeapFinished(request: RequestId, taskId: TaskId): Unit = inHeapFinished.add((request, taskId)): Unit
  def forkSpawned(fork: ForkId, pid: Long): Unit = spawned.add((fork, pid)): Unit
  def forkWorkFinished(fork: ForkId, cpu: Int): Unit = workFinished.add((fork, cpu)): Unit
  def forkExited(fork: ForkId): Unit = exited.add(fork): Unit
}
