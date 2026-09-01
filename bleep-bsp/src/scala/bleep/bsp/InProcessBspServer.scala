package bleep.bsp

import cats.effect.{IO, Resource}

import scala.concurrent.duration.*
import ryddig.Logger

import java.util.concurrent.CompletableFuture

/** Creates an in-process BSP server connected through a pair of [[InMemoryPipe]]s.
  *
  * Used by integration tests to avoid launching a separate JVM process. The BSP server runs in a daemon thread within the same JVM.
  */
object InProcessBspServer {

  /** An in-process server, given the config it should use and a governor it should share.
    *
    * Both used to be invented here: the config was read from the developer's own `config.yaml` per request, and a `MachineResources` was built `forThisMachine`
    * — sized for every core and most of the RAM — once per connection. A test suite running many of these concurrently therefore had as many governors as
    * connections, each admitting forks as though it were the machine's only tenant, which is the opposite of what a governor is for.
    *
    * Callers now supply both, so a suite can share one governor across everything it runs and mean what it says about heaps and parallelism.
    */
  def connect(config: bleep.model.BleepConfig, machine: bleep.MachineResources)(logger: Logger): Resource[IO, BspConnection] =
    Resource.make(
      IO.blocking {
        // Two pipes carry the two directions. A megabyte of slack keeps a sourcegen run that logs faster than the
        // client reads from blocking the server.
        val clientToServer = new InMemoryPipe(1048576)
        val serverToClient = new InMemoryPipe(1048576)

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
              val inProcessAnalysisCache = new bleep.analysis.AnalysisCache
              // One server per in-process run, so fresh daemon-scoped state is correct here.
              val server =
                new MultiWorkspaceBspServer(
                  clientToServer.source,
                  serverToClient.sink,
                  logger,
                  machine = machine,
                  heapMonitor = HeapMonitor.system,
                  kspMutexes = new KspMutexes,
                  buildCache =
                    new BuildCache(bleep.model.BspServerConfig.default.maxCachedWorkspacesFor(Runtime.getRuntime.maxMemory()), inProcessAnalysisCache),
                  analysisCache = inProcessAnalysisCache,
                  daemonInfo = DaemonInfo.inProcess(config.bspServerConfigOrDefault),
                  connId = 1,
                  configOverride = Some(config)
                )
              server.run()
            } catch {
              case e: Throwable =>
                exitCode = 1
                logger.error(s"In-process BSP server failed: ${e.getClass.getName}: ${e.getMessage}", e)
            } finally {
              try serverToClient.sink.close()
              catch { case _: Exception => () }
              try clientToServer.source.close()
              catch { case _: Exception => () }
              exited.complete(exitCode): Unit
            }
          }
        }
        serverThread.start()

        new InProcessConnection(serverToClient.source, clientToServer.sink, exited)
      }
    )(_.close)

  private class InProcessConnection(
      val input: java.io.InputStream,
      val output: java.io.OutputStream,
      exited: CompletableFuture[java.lang.Integer]
  ) extends BspConnection {
    def serverExited: IO[Int] = IO.fromCompletableFuture(IO.pure(exited)).map(_.intValue)

    /** Close the transport, then wait for the server thread to finish unwinding.
      *
      * Closing the pipes is what tells `run()` to return, but nothing used to wait for it — so a test could finish, and start the next one, while the previous
      * server was still cancelling requests and killing its child processes. The wait is bounded: a server that will not come down must not hang the suite that
      * is trying to leave.
      */
    def close: IO[Unit] =
      IO.blocking {
        try output.close()
        catch { case _: Exception => () }
        try input.close()
        catch { case _: Exception => () }
      } >> IO
        .fromCompletableFuture(IO.pure(exited))
        .timeout(30.seconds)
        .void
        .handleError(_ => ())
  }
}
