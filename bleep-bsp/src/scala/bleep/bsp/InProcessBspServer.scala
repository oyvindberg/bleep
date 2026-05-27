package bleep.bsp

import cats.effect.{IO, Resource}
import ryddig.Logger

import java.io.{PipedInputStream, PipedOutputStream}
import java.util.concurrent.CompletableFuture

/** Creates an in-process BSP server connected via piped streams.
  *
  * Used by integration tests to avoid launching a separate JVM process. The BSP server runs in a daemon thread within the same JVM.
  */
object InProcessBspServer {

  def connect(logger: Logger): Resource[IO, BspConnection] =
    Resource.make(
      IO.blocking {
        // Create two pipe pairs for bidirectional communication
        val serverIn = new PipedInputStream(1048576) // 1MB buffer to prevent deadlocks during sourcegen
        val clientOut = new PipedOutputStream(serverIn) // client writes -> server reads
        val clientIn = new PipedInputStream(1048576) // 1MB buffer
        val serverOut = new PipedOutputStream(clientIn) // server writes -> client reads

        // Use CompletableFuture (not a Deferred) so the server thread can signal exit without
        // bouncing through cats-effect from a non-IO thread. IO.fromCompletableFuture bridges
        // it back into IO for callers.
        val exited = new CompletableFuture[java.lang.Integer]()

        // Start BSP server in a daemon thread
        val serverThread = new Thread("in-process-bsp-server") {
          setDaemon(true)
          override def run(): Unit = {
            var exitCode: java.lang.Integer = 0
            try {
              val semaphore = new java.util.concurrent.Semaphore(Runtime.getRuntime.availableProcessors(), /* fair = */ true)
              val server =
                new MultiWorkspaceBspServer(serverIn, serverOut, logger, compileSemaphore = semaphore, heapMonitor = HeapMonitor.system)
              server.run()
            } catch {
              case e: Throwable =>
                exitCode = 1
                logger.error(s"In-process BSP server failed: ${e.getClass.getName}: ${e.getMessage}", e)
            } finally {
              try serverOut.close()
              catch { case _: Exception => () }
              try serverIn.close()
              catch { case _: Exception => () }
              exited.complete(exitCode): Unit
            }
          }
        }
        serverThread.start()

        new InProcessConnection(clientIn, clientOut, exited)
      }
    )(_.close)

  private class InProcessConnection(
      val input: java.io.InputStream,
      val output: java.io.OutputStream,
      exited: CompletableFuture[java.lang.Integer]
  ) extends BspConnection {
    def serverExited: IO[Int] = IO.fromCompletableFuture(IO.pure(exited)).map(_.intValue)
    def close: IO[Unit] = IO.blocking {
      try output.close()
      catch { case _: Exception => () }
      try input.close()
      catch { case _: Exception => () }
    }
  }
}
