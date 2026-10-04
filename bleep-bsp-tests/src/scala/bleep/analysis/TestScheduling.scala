package bleep.analysis

import bleep.bsp.{DaemonScheduling, HeapWaitListener, RequestChannel}
import bleep.machine.{HeapGate, HeapUsage, RequestId, RequestKind, TaskId}
import ryddig.TypedLogger

/** A scheduler for DAG tests: unconstrained, with the parallelism a case asks for, so admission is exercised through the real executor path without probes,
  * locks or state files. One scheduler per channel; its tick thread is a daemon thread that parks when the request is gone.
  */
object TestScheduling {
  val noHeapWaits: HeapWaitListener = new HeapWaitListener {
    def onWait(taskId: TaskId, heap: HeapUsage, delayMs: Long, nowMs: Long): Unit = ()
    def onResume(taskId: TaskId, heap: HeapUsage, waitedForMs: Long, nowMs: Long): Unit = ()
  }

  def openChannel(parallelism: Int, heapGate: HeapGate): RequestChannel = {
    val scheduling = DaemonScheduling.unconstrained(parallelism, "dag test", heapGate, TypedLogger.DevNull)
    scheduling.openRequest(RequestId(java.util.UUID.randomUUID().toString), RequestKind.Test, noHeapWaits)
  }

  def openChannel(parallelism: Int): RequestChannel = openChannel(parallelism, HeapGate.alwaysAdmit)
}
