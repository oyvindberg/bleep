package bleep.bsp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets

/** These tests assert what [[InMemoryPipe]] does that `java.io.PipedInputStream` does not.
  *
  * The BSP server writes replies and notifications from Cats Effect blocking regions. Cats Effect retires the carrier thread of such a region. A pipe that ends
  * its reader when a writing thread exits loses every message written after that point.
  */
class InMemoryPipeTest extends AnyFunSuite with Matchers {

  private def readFully(pipe: InMemoryPipe, length: Int): String = {
    val destination = new Array[Byte](length)
    var read = 0
    while (read < length) {
      val taken = pipe.source.read(destination, read, length - read)
      if (taken < 0) fail(s"The pipe reached its end after $read of $length bytes")
      read += taken
    }
    new String(destination, StandardCharsets.UTF_8)
  }

  test("a read after the writing thread exits waits for the next writer") {
    val pipe = new InMemoryPipe(64)

    val first = new Thread(() => pipe.sink.write("first".getBytes(StandardCharsets.UTF_8)), "first-writer")
    first.start()
    first.join()

    readFully(pipe, 5) shouldBe "first"

    // `java.io.PipedInputStream` throws `Write end dead` here. This read finds the pipe empty and waits.
    val reader = new Thread(() => readFully(pipe, 6): Unit, "waiting-reader")
    reader.setDaemon(true)
    reader.start()
    reader.join(500)
    reader.isAlive shouldBe true

    val second = new Thread(() => pipe.sink.write("second".getBytes(StandardCharsets.UTF_8)), "second-writer")
    second.start()
    second.join()
    reader.join(5000)
    reader.isAlive shouldBe false
  }

  test("bytes arrive in the order they were written across a wrap of the buffer") {
    val pipe = new InMemoryPipe(8)

    pipe.sink.write("abcdef".getBytes(StandardCharsets.UTF_8))
    readFully(pipe, 6) shouldBe "abcdef"

    pipe.sink.write("ghijkl".getBytes(StandardCharsets.UTF_8))
    readFully(pipe, 6) shouldBe "ghijkl"
  }

  test("a read returns the end of the stream once the sink closes and the buffer empties") {
    val pipe = new InMemoryPipe(64)

    pipe.sink.write("last".getBytes(StandardCharsets.UTF_8))
    pipe.sink.close()

    readFully(pipe, 4) shouldBe "last"
    pipe.source.read(new Array[Byte](4), 0, 4) shouldBe -1
  }

  test("a write after the source closes raises PipeClosedByReaderException") {
    val pipe = new InMemoryPipe(64)
    pipe.source.close()

    a[PipeClosedByReaderException] should be thrownBy pipe.sink.write("dropped".getBytes(StandardCharsets.UTF_8))
  }

  test("a write larger than the capacity crosses to a reader that drains as it goes") {
    val pipe = new InMemoryPipe(16)
    val payload = ("0123456789" * 40).getBytes(StandardCharsets.UTF_8)

    val writer = new Thread(() => pipe.sink.write(payload), "bulk-writer")
    writer.setDaemon(true)
    writer.start()

    readFully(pipe, payload.length) shouldBe new String(payload, StandardCharsets.UTF_8)
    writer.join(5000)
    writer.isAlive shouldBe false
  }
}
