package bleep.bsp

import cats.effect.{IO, Resource}
import ryddig.Logger

import java.net.{InetAddress, ServerSocket, Socket}
import java.util.concurrent.CompletableFuture

/** Creates an in-process BSP server connected via piped streams.
  *
  * Used by integration tests to avoid launching a separate JVM process. The BSP server runs in a daemon thread within the same JVM.
  */
object InProcessBspServer {

  def connect(logger: Logger): Resource[IO, BspConnection] =
    Resource.make(
      IO.blocking {
        // A loopback socket pair, not piped streams.
        //
        // `PipedInputStream` remembers the last thread that read from it and the last thread that wrote to it, and once either of those threads dies it refuses
        // to go on — "Read end dead" from the writer, "Write end dead" from the reader. Both ends here live on cats-effect pools: reads ran on
        // `io-compute-blocker-N` and writes on `io-compute-N`, both picking a different thread from call to call, and blocker threads are created on demand and
        // retired once idle. So a perfectly healthy connection would break the moment the pool happened to retire a thread that had touched it, leaving the
        // client parked in `CompletableFuture.get()` waiting for a reply and the server parked in `read` waiting for the request — which is what running
        // several build-driving integration tests at once reliably produced, and what any timing change made disappear.
        //
        // Sockets have no such affinity (`PipedStreamThreadAffinityTest` pins both halves of that), and they are what the out-of-process server already speaks,
        // so the in-process path now differs from the real one in one fewer respect. Bound to the loopback address so nothing outside the machine can reach it,
        // and the listener is closed as soon as both ends are connected.
        val listener = new ServerSocket(0, 1, InetAddress.getLoopbackAddress)
        val (clientSocket, serverSocket) =
          try {
            // Backlog of 1 is enough for the connect to complete into the queue before `accept` runs, so doing both on this thread cannot deadlock.
            val client = new Socket(InetAddress.getLoopbackAddress, listener.getLocalPort)
            val server = listener.accept()
            client.setTcpNoDelay(true)
            server.setTcpNoDelay(true)
            (client, server)
          } finally listener.close()

        val serverIn = serverSocket.getInputStream
        val clientOut = clientSocket.getOutputStream
        val clientIn = clientSocket.getInputStream
        val serverOut = serverSocket.getOutputStream

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
              val numCores = Runtime.getRuntime.availableProcessors()
              val machine = bleep.MachineResources.forThisMachine(totalCpu = numCores, logger = logger)
              val inProcessAnalysisCache = new bleep.analysis.AnalysisCache
              // One server per in-process run, so fresh daemon-scoped state is correct here.
              val server =
                new MultiWorkspaceBspServer(
                  serverIn,
                  serverOut,
                  logger,
                  machine = machine,
                  heapMonitor = HeapMonitor.system,
                  kspMutexes = new KspMutexes,
                  buildCache =
                    new BuildCache(bleep.model.BspServerConfig.default.maxCachedWorkspacesFor(Runtime.getRuntime.maxMemory()), inProcessAnalysisCache),
                  analysisCache = inProcessAnalysisCache,
                  daemonInfo = DaemonInfo.inProcess(bleep.model.BspServerConfig.default),
                  connId = 1
                )
              server.run()
            } catch {
              case e: Throwable =>
                exitCode = 1
                logger.error(s"In-process BSP server failed: ${e.getClass.getName}: ${e.getMessage}", e)
            } finally {
              try serverSocket.close()
              catch { case _: Exception => () }
              exited.complete(exitCode): Unit
            }
          }
        }
        serverThread.start()

        new InProcessConnection(clientIn, clientOut, clientSocket, exited)
      }
    )(_.close)

  private class InProcessConnection(
      val input: java.io.InputStream,
      val output: java.io.OutputStream,
      clientSocket: Socket,
      exited: CompletableFuture[java.lang.Integer]
  ) extends BspConnection {
    def serverExited: IO[Int] = IO.fromCompletableFuture(IO.pure(exited)).map(_.intValue)
    def close: IO[Unit] = IO.blocking {
      // Closing the socket closes both of its streams, and gives the server a clean end-of-input rather than a stream that merely stops producing.
      try clientSocket.close()
      catch { case _: Exception => () }
    }
  }
}
