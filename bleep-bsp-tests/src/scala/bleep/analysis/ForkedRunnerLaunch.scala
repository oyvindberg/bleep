package bleep.analysis

import java.io.{BufferedReader, InputStreamReader, OutputStreamWriter, PrintWriter}
import java.net.{InetAddress, ServerSocket, Socket}
import java.nio.charset.StandardCharsets

/** Launches `bleep.testing.runner.ForkedTestRunner` the way bleep itself does, for tests that drive it directly.
  *
  * The runner speaks its protocol over a loopback socket the launcher is already listening on. It does not use stdio: sharing stdout with the protocol meant
  * that anything writing to file descriptor 1 — a subprocess started with inherited IO, which is how Scala Native's test binaries are spawned — landed inside
  * the JSON stream and killed the run. See `JvmPool` for the production side of the same handshake.
  */
object ForkedRunnerLaunch {

  case class Launched(process: Process, reader: BufferedReader, writer: PrintWriter, socket: Socket, private val drained: StringBuffer) {

    /** Everything the runner has written to stderr so far. Worth having in a failure message: when the handshake does not arrive, this is where the reason is.
      */
    def stderr: String = drained.toString

    def close(): Unit = {
      process.destroyForcibly()
      reader.close()
      writer.close()
      socket.close()
    }
  }

  def launch(javaBin: String, classpath: String): Launched = {
    val listener = new ServerSocket(0, 1, InetAddress.getLoopbackAddress)
    val process =
      try
        new ProcessBuilder(
          javaBin,
          s"-D${bleep.testing.ForkedTestRunnerProtocol.PortProperty}=${listener.getLocalPort}",
          "-cp",
          classpath,
          "bleep.testing.runner.ForkedTestRunner"
        ).start()
      catch {
        case e: Throwable =>
          listener.close()
          throw e
      }

    val socket =
      try {
        // Bounded so a runner that dies during startup fails the test with its own stderr rather than hanging it.
        listener.setSoTimeout(60000)
        listener.accept()
      } catch {
        case e: Throwable =>
          process.destroyForcibly(): Unit
          throw e
      } finally listener.close()
    socket.setTcpNoDelay(true)

    // Both of the process's own streams are drained on background threads. Nothing else reads them now that the protocol has moved off stdio, and an
    // undrained pipe fills up and blocks the child — silently, which is the worst way for a test to fail.
    val collected = new StringBuffer
    drain(s"forked-runner-stderr-${process.pid}", process.getErrorStream, Some(collected))
    drain(s"forked-runner-stdout-${process.pid}", process.getInputStream, None)

    Launched(
      process = process,
      reader = new BufferedReader(new InputStreamReader(socket.getInputStream, StandardCharsets.UTF_8)),
      writer = new PrintWriter(new OutputStreamWriter(socket.getOutputStream, StandardCharsets.UTF_8), true),
      socket = socket,
      drained = collected
    )
  }

  private def drain(name: String, stream: java.io.InputStream, into: Option[StringBuffer]): Unit = {
    val t = new Thread(name) {
      override def run(): Unit =
        try {
          val reader = new BufferedReader(new InputStreamReader(stream))
          var line = reader.readLine()
          while (line != null) {
            into.foreach(sb => sb.append(line).append('\n'))
            line = reader.readLine()
          }
        } catch { case _: Throwable => () }
    }
    t.setDaemon(true)
    t.start()
  }
}
