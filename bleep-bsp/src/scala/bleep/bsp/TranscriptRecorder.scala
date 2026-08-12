package bleep.bsp

import bleep.bsp.protocol.BleepBspProtocol

import scala.jdk.CollectionConverters.*

/** Per-request accumulator for the protocol events streamed while serving one `buildTarget/compile` or `buildTarget/test` request.
  *
  * Created at the top of the request handler and passed structurally through the call chain — the compile handler, the heap-pressure listener, the DAG event
  * consumers — so every event that goes out to the client as a `build/taskProgress` notification is also captured server-side, in send order. At request
  * completion the accumulated stream becomes the body of the [[bleep.requests.Transcript]] the daemon persists.
  *
  * Thread-safe because a request's events are produced concurrently (compiler callbacks, DAG consumer fiber, lock-contention callbacks); scope stays strictly
  * per-request — this is a parameter, never shared state.
  */
final class TranscriptRecorder {
  private val queue = new java.util.concurrent.ConcurrentLinkedQueue[BleepBspProtocol.Event]

  def record(event: BleepBspProtocol.Event): Unit = {
    queue.add(event)
    ()
  }

  /** The events recorded so far, in the order they were sent. */
  def events: List[BleepBspProtocol.Event] = queue.iterator.asScala.toList
}
