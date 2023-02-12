package bleep
package bsp

import ch.epfl.scala.bsp4j
import org.eclipse.lsp4j.jsonrpc

import java.net.Socket
import java.nio.file.{Files, Path}
import scala.build.bloop.{BloopServer, BloopThreads, BuildServer}
import scala.build.blooprifle.internal.Operations
import scala.build.blooprifle.{BloopRifle, BloopRifleConfig, BloopRifleLogger, FailedToStartServerException}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

object BspImpl {
  def run(pre: Prebootstrapped): ExitCode = {
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

      val config = BleepConfigOps.loadOrDefault(pre.userPaths).orThrow
      val build = pre.existingBuild.buildFile.forceGet.orThrow
      val bleepRifleLogger = new BleepRifleLogger(pre.logger)
      val resolver = CoursierResolver.Factory.default(pre, config, build)
      val bloopRifleConfig =
        SetupBloopRifle(
          compileServerMode = config.compileServerModeOrDefault,
          resolvedJvm = pre.resolvedJvm.forceGet,
          userPaths = pre.userPaths,
          resolver = resolver,
          bleepExecutable = Lazy(BleepExecutable.getCommand(resolver, pre, forceJvm = false)),
          bleepRifleLogger = bleepRifleLogger
        )

      val workspaceDir = pre.buildPaths.buildVariantDir

      var bloopServer: BloopServer = null
      try {
        bloopServer = buildServer(bloopRifleConfig, workspaceDir, localClient, threads.buildThreads, bleepRifleLogger)
        bspBloopServer.sendToIdeClient = launcher.getRemoteProxy
        bspBloopServer.bloopServer = bloopServer.server
        // run on `workspaceBuildTargets` later for the side-effect of updating build
        bspBloopServer.buildChangeTracker = BuildChangeTracker.make(pre, localClient)

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
        ExitCode.Success
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
      try BloopServer.bsp(config, workspace, threads, logger, config.period, config.timeout)
      catch {
        case th: FailedToStartServerException =>
          val readLog: Option[String] =
            config.address match {
              case _: BloopRifleConfig.Address.Tcp           => None
              case ds: BloopRifleConfig.Address.DomainSocket => Try(Files.readString(ds.outputPath)).toOption
            }
          throw BleepCommandRemote.FailedToStartBloop(th, readLog)

      }

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
