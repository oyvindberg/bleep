package bleep.bsp

import cats.effect.{IO, Resource}
import ryddig.Logger

import java.io.{PipedInputStream, PipedOutputStream}

/** Creates an in-process BSP server connected via piped streams.
  *
  * Used by integration tests to avoid launching a separate JVM process. The BSP server runs in a daemon thread within the same JVM.
  */
object InProcessBspServer {

  def connect(logger: Logger): Resource[IO, BspConnection] =
    Resource.make(
      IO.blocking {
        // Create two pipe pairs for bidirectional communication
        val serverIn = new PipedInputStream(65536)
        val clientOut = new PipedOutputStream(serverIn) // client writes -> server reads
        val clientIn = new PipedInputStream(65536)
        val serverOut = new PipedOutputStream(clientIn) // server writes -> client reads

        // Start BSP server in a daemon thread
        val serverThread = new Thread("in-process-bsp-server") {
          setDaemon(true)
          override def run(): Unit =
            try {
              val semaphore = new java.util.concurrent.Semaphore(Runtime.getRuntime.availableProcessors())
              val server =
                new MultiWorkspaceBspServer(serverIn, serverOut, logger, socketDir = None, compileSemaphore = semaphore, heapMonitor = HeapMonitor.system)
              server.run()
            } catch {
              case _: Exception => ()
            } finally {
              try serverOut.close()
              catch { case _: Exception => () }
              try serverIn.close()
              catch { case _: Exception => () }
            }
        }
        serverThread.start()

        new InProcessConnection(clientIn, clientOut)
      }
    )(_.close)

  private class InProcessConnection(
      val input: java.io.InputStream,
      val output: java.io.OutputStream
  ) extends BspConnection {
    def serverExited: IO[Int] = IO.never
    def close: IO[Unit] = IO.blocking {
      try output.close()
      catch { case _: Exception => () }
      try input.close()
      catch { case _: Exception => () }
    }
  }
}
