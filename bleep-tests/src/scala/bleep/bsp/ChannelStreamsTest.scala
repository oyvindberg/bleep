package bleep.bsp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.channels.{ServerSocketChannel, SocketChannel}
import java.nio.file.Files
import java.util.concurrent.{CompletableFuture, TimeUnit}

/** A BSP client reads and writes one socket channel from two threads: the JSON-RPC reader waits in `read` while the caller sends its first request. On JDK 17,
  * `Channels.newInputStream`/`newOutputStream` share one lock, so that write never happened. This JVM is newer, where the old streams work too, so this pins
  * the contract rather than reproducing the hang; the JDK 17 builds that hung are what showed it.
  */
class ChannelStreamsTest extends AnyFunSuite with Matchers {

  private def withPair[A](f: (SocketChannel, SocketChannel) => A): A = {
    val dir = Files.createTempDirectory("channel-streams")
    val address = UnixDomainSocketAddress.of(dir.resolve("s"))
    val server = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
    try {
      server.bind(address)
      val client = SocketChannel.open(address)
      val accepted = server.accept()
      try f(client, accepted)
      finally { client.close(); accepted.close() }
    } finally server.close()
  }

  test("a write goes out while another thread waits in read") {
    withPair { (client, peer) =>
      val in = ChannelStreams.input(client)
      val out = ChannelStreams.output(client)
      // the reader, waiting for the peer's first message
      val received = CompletableFuture.supplyAsync(() => new String(in.readNBytes(5)))
      Thread.sleep(200)
      out.write("request".getBytes)
      out.flush()
      new String(ChannelStreams.input(peer).readNBytes(7)) shouldBe "request"
      ChannelStreams.output(peer).write("reply".getBytes)
      received.get(10, TimeUnit.SECONDS) shouldBe "reply"
    }
  }

  test("end of stream reads as -1") {
    withPair { (client, peer) =>
      peer.close()
      ChannelStreams.input(client).read() shouldBe -1
      ChannelStreams.input(client).read(new Array[Byte](4), 0, 4) shouldBe -1
    }
  }
}
