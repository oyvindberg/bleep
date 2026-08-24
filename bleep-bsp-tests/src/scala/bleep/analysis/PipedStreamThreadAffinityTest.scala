package bleep.analysis

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.io.{IOException, PipedInputStream, PipedOutputStream}

/** Why `InProcessBspServer` does not join its two ends with piped streams.
  *
  * `PipedInputStream` remembers the last thread to read from it and the last thread to write to it, and refuses to continue once either of those threads has
  * died — regardless of whether anything is still using the stream. That is fine for the two-dedicated-threads arrangement the class was designed for, and
  * wrong for any arrangement where the work moves between pooled threads, which is what cats-effect does: reads landed on `io-compute-blocker-N` and writes on
  * `io-compute-N`, both rotating call to call, and blocker threads are created on demand and retired when idle.
  *
  * The symptom was a BSP client parked in `CompletableFuture.get()` waiting for a reply while the server sat in `PipedInputStream.read` waiting for a request
  * neither of them could still deliver — reproducible by running several build-driving integration tests at once, and vanishing as soon as anything perturbed
  * the timing.
  *
  * These tests pin the behaviour rather than the workaround: if a future JDK stops tracking thread liveness this way, they fail and the socket pair the
  * transport now uses can be reconsidered.
  */
class PipedStreamThreadAffinityTest extends AnyFunSuite with Matchers {

  test("a write fails once the thread that last read has died") {
    val in = new PipedInputStream(1024)
    val out = new PipedOutputStream(in)

    out.write('a')
    val reader = new Thread(() => in.read(): Unit, "short-lived-reader")
    reader.start()
    reader.join()

    // The reader is gone but the pipe is still perfectly usable as far as any caller knows: the buffer is empty, both ends are open, and another thread is
    // about to read. The write fails anyway.
    val thrown = intercept[IOException](out.write('b'))
    thrown.getMessage shouldBe "Read end dead"
  }

  test("a read fails once the thread that last wrote has died") {
    val in = new PipedInputStream(1024)
    val out = new PipedOutputStream(in)

    val writer = new Thread(() => out.write('a'), "short-lived-writer")
    writer.start()
    writer.join()

    // Draining what the dead thread wrote is fine — the check only bites when the buffer runs empty and the reader would have to wait.
    in.read() shouldBe 'a'

    val thrown = intercept[IOException](in.read())
    thrown.getMessage shouldBe "Write end dead"
  }

  test("a socket pair has no such thread affinity") {
    val listener = new java.net.ServerSocket(0, 1, java.net.InetAddress.getLoopbackAddress)
    val clientEnd = new java.net.Socket(java.net.InetAddress.getLoopbackAddress, listener.getLocalPort)
    val serverEnd = listener.accept()
    listener.close()

    try {
      val writer = new Thread(() => clientEnd.getOutputStream.write('a'), "short-lived-writer")
      writer.start()
      writer.join()

      val reader = new Thread(() => serverEnd.getInputStream.read(): Unit, "short-lived-reader")
      reader.start()
      reader.join()

      // Both threads that touched the connection are dead, and it still works from a third.
      clientEnd.getOutputStream.write('b')
      serverEnd.getInputStream.read() shouldBe 'b'
    } finally {
      clientEnd.close()
      serverEnd.close()
    }
  }
}
