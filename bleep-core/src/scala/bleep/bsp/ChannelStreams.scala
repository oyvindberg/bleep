package bleep.bsp

import java.io.{InputStream, OutputStream}
import java.nio.ByteBuffer
import java.nio.channels.SocketChannel

/** Streams over a blocking `SocketChannel`, reading and writing the channel directly.
  *
  * Not `Channels.newInputStream` / `newOutputStream`: on JDK 17 both synchronise on the channel's one blocking lock, so a thread sitting in `read` — the
  * JSON-RPC reader, waiting for the server's first message — keeps every `write` out, and the client never sends the request the server is waiting for. Newer
  * JDKs dropped that lock for socket channels, which is why only a client on JDK 17 hung; a script calling back into bleep runs on the build's JVM, which can
  * be 17. `SocketChannel.read` and `write` take separate locks on every JDK, so one thread can wait for input while another writes.
  */
object ChannelStreams {
  def input(channel: SocketChannel): InputStream = new InputStream {
    override def read(): Int = {
      val one = new Array[Byte](1)
      read(one, 0, 1) match {
        case -1 => -1
        case _  => one(0) & 0xff
      }
    }
    // A blocking channel returns at least one byte, or -1 at end of stream.
    override def read(b: Array[Byte], off: Int, len: Int): Int =
      if (len == 0) 0 else channel.read(ByteBuffer.wrap(b, off, len))
    override def close(): Unit = channel.close()
  }

  def output(channel: SocketChannel): OutputStream = new OutputStream {
    override def write(b: Int): Unit = write(Array(b.toByte), 0, 1)
    override def write(b: Array[Byte], off: Int, len: Int): Unit = {
      val buffer = ByteBuffer.wrap(b, off, len)
      while (buffer.hasRemaining) channel.write(buffer): Unit
    }
    override def close(): Unit = channel.close()
  }
}
