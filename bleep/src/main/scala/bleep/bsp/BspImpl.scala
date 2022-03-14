package bleep
package bsp

import bleep.internal.Lazy
import ch.epfl.scala.bsp4j
import org.eclipse.lsp4j.jsonrpc

import java.net.Socket
import java.nio.file.{Files, Path}
import scala.build.bloop.{BloopServer, BloopThreads, BuildServer}
import scala.build.blooprifle.internal.Operations
import scala.build.blooprifle.{BloopRifleConfig, BloopRifleLogger, BloopServerRuntimeInfo}
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

      val lazyResolver = Lazy {
        val resolvers = model.parseBuild(Files.readString(pre.buildPaths.bleepJsonFile)) match {
          case Left(th) =>
            pre.logger.warn("Cannot setup custom resolvers since build is broken", th)
            Nil
          case Right(build) => build.resolvers.values
        }
        CoursierResolver(resolvers, pre.logger, downloadSources = false, pre.userPaths)
      }

      val bloopRifleConfig =
        CompileServerConfig.load(pre.userPaths) match {
          case Left(th) => throw th
          case Right(config) =>
            SetupBloopRifle(JavaCmd.javacommand, pre.buildPaths, lazyResolver, config.asAddress(pre.userPaths))
        }

      val bloopRifleLogger = new BloopLogger(pre.logger)

      val workspaceDir = pre.buildPaths.bleepBloopDir.getParent

      var bloopServer: BloopServer = null
      try {
        bloopServer = buildServer(bloopRifleConfig, workspaceDir, localClient, threads.buildThreads, bloopRifleLogger)
        bspBloopServer.sendToIdeClient = launcher.getRemoteProxy
        bspBloopServer.bloopServer = bloopServer.server
        // run on `workspaceBuildTargets` later for the side-effect of updating build
        bspBloopServer.ensureBloopUpToDate = () =>
          ProjectSelection.load(pre.buildPaths).flatMap { maybeSelectedProjectGlobs =>
            val bspRewrites: List[Rewrite] = List(
//              List(rewrites.semanticDb(pre.buildPaths)),
              maybeSelectedProjectGlobs match {
                case Some(selectedProjectGlobs) => List(rewrites.keepSelectedProjects(selectedProjectGlobs))
                case None                       => Nil
              }
            ).flatten

            bootstrap.from(pre, bspRewrites)
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
          System.in,
          System.out,
          System.err,
          bloopRifleLogger
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
        bloopInfo: BloopServerRuntimeInfo
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
