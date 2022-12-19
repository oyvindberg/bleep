package bleep
package bsp

import bleep.Lazy
import bleep.internal.jvmOrSystem
import bleep.rewrites.BuildRewrite
import ch.epfl.scala.bsp4j
import org.eclipse.lsp4j.jsonrpc

import java.net.Socket
import java.nio.file.Path
import scala.build.bloop.{BloopServer, BloopThreads, BuildServer}
import scala.build.blooprifle.internal.Operations
import scala.build.blooprifle.{BloopRifle, BloopRifleConfig, BloopRifleLogger}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

object BspImpl {
  def run(pre: Prebootstrapped): Unit = {

    val bspBloopServer: BleepBspServer =
      new BleepBspServer(pre.logger, null, null, null)

    bsp.BspThreads.withThreads { threads =>
      val launcher = new jsonrpc.Launcher.Builder[bsp4j.BuildClient]()
        .setExecutorService(threads.buildThreads.jsonrpc)
        .setInput(System.in)
        .setOutput(System.out)
        .setRemoteInterface(classOf[bsp4j.BuildClient])
        .setLocalService(bspBloopServer)
        .create()

      val localClient = new BspForwardClient(Some(launcher.getRemoteProxy))

      val bleepConfig = BleepConfigOps.loadOrDefault(pre.userPaths).orThrow
      val build = pre.existingBuild.buildFile.forceGet.orThrow
      val bleepRifleLogger = new BleepRifleLogger(pre.logger)

      val bloopRifleConfig =
        SetupBloopRifle(
          compileServerMode = bleepConfig.compileServerMode,
          jvm = jvmOrSystem(build.jvm, pre.logger),
          logger = pre.logger,
          userPaths = pre.userPaths,
          resolver = Lazy(CoursierResolver.Factory.default(pre, bleepConfig, build)),
          bleepRifleLogger = bleepRifleLogger,
          executionContext = ExecutionContext.fromExecutor(threads.prepareBuildExecutor)
        )

      val workspaceDir = pre.buildPaths.buildVariantDir

      var bloopServer: BloopServer = null
      try {
        bloopServer = buildServer(bloopRifleConfig, workspaceDir, localClient, threads.buildThreads, bleepRifleLogger)
        bspBloopServer.sendToIdeClient = launcher.getRemoteProxy
        bspBloopServer.bloopServer = bloopServer.server
        // run on `workspaceBuildTargets` later for the side-effect of updating build
        bspBloopServer.ensureBloopUpToDate = () =>
          BspProjectSelection.load(pre.buildPaths).flatMap { maybeSelectedProjectGlobs =>
            val bspRewrites: List[BuildRewrite] = List(
//              List(rewrites.semanticDb(pre.buildPaths)),
              maybeSelectedProjectGlobs match {
                case Some(selectedProjectGlobs) => List(rewrites.keepSelectedProjects(selectedProjectGlobs))
                case None                       => Nil
              }
            ).flatten

            pre.fresh.flatMap { pre =>
              bootstrap.from(pre, GenBloopFiles.SyncToDisk, bspRewrites, Lazy(bleepConfig), CoursierResolver.Factory.default)
            }
          }

        pre.logger.info {
          val hasConsole = System.console() != null
          if (hasConsole)
            "Listening to incoming JSONRPC BSP requests, press Ctrl+D to exit."
          else
            "Listening to incoming JSONRPC BSP requests."
        }

        val es = ExecutionContext.fromExecutorService(threads.buildThreads.jsonrpc)
        val doneFuture = Future.firstCompletedOf(
          Seq(
            BspImpl.naiveJavaFutureToScalaFuture(launcher.startListening()).map(_ => ())(es),
            bspBloopServer.initiateShutdown
          )
        )(es)

        Await.result(doneFuture, Duration.Inf)
      } finally {
        if (bloopServer != null)
          bloopServer.shutdown()

        Operations.exit(
          bloopRifleConfig.address,
          workspaceDir,
          System.out,
          System.err,
          bleepRifleLogger
        )
        ()
      }
    }
  }

  // from https://githubsp4j.com/com-lihaoyi/Ammonite/blob/7eb58c58ec8c252dc5bd1591b041fcae01cccf90/amm/interp/src/main/scala/ammonite/interp/script/AmmoniteBuildServer.scala#L550-L565
  private def naiveJavaFutureToScalaFuture[T](f: java.util.concurrent.Future[T]): Future[T] = {
    val p = Promise[T]()
    val t = new Thread {
      setDaemon(true)
      setName("bsp-wait-for-exit")
      override def run(): Unit =
        p.complete {
          try Success(f.get())
          catch { case t: Throwable => Failure(t) }
        }
    }
    t.start()
    p.future
  }

  def buildServer(config: BloopRifleConfig, workspace: Path, buildClient: bsp4j.BuildClient, threads: BloopThreads, logger: BloopRifleLogger): BloopServer = {

    val (conn, socket, bloopInfo) =
      BloopServer.bsp(config, workspace, threads, logger, config.period, config.timeout)

    logger.debug(s"Connected to Bloop via BSP at ${conn.address}")

    // FIXME As of now, we don't detect when connection gets closed.
    // For TCP connections, this should be do-able with heartbeat messages
    // (to be added to BSP?).
    // For named sockets, the recv system call is supposed to allow to detect
    // that case, unlike the read system call. But the ipcsocket library that we use
    // for named sockets relies on read.

    val launcher = new jsonrpc.Launcher.Builder[BuildServer]()
      .setExecutorService(threads.jsonrpc)
      .setInput(socket.getInputStream)
      .setOutput(socket.getOutputStream)
      .setRemoteInterface(classOf[BuildServer])
      .setLocalService(buildClient)
      .create()
    val server = launcher.getRemoteProxy
    buildClient.onConnectWithServer(server)

    val f = launcher.startListening()

    case class BloopServerImpl(
        server: BuildServer,
        listeningFuture: java.util.concurrent.Future[Void],
        socket: Socket,
        bloopInfo: BloopRifle.BloopServerRuntimeInfo
    ) extends BloopServer {
      def shutdown(): Unit = {
        // Close the jsonrpc thread listening to input messages
        // First line makes jsonrpc discard the closed connection exception.
        listeningFuture.cancel(true)
        socket.close()
      }
    }
    BloopServerImpl(server, f, socket, bloopInfo)
  }

}
