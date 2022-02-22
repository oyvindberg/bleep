package bleep
package bsp

import bleep.logging._
import ch.epfl.scala.bsp4j
import org.eclipse.lsp4j.jsonrpc

import scala.build.bloop.BloopServer
import scala.build.blooprifle.internal.Operations
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

object BspImpl {
  def run(buildPaths: BuildPaths, logger: Logger): Unit = {

    val bspBloopServer: BleepBspServer =
      new BleepBspServer(logger, null, null)

    bsp.BspThreads.withThreads { threads =>
      val launcher = new jsonrpc.Launcher.Builder[bsp4j.BuildClient]()
        .setExecutorService(threads.buildThreads.jsonrpc)
        .setInput(System.in)
        .setOutput(System.out)
        .setRemoteInterface(classOf[bsp4j.BuildClient])
        .setLocalService(bspBloopServer)
        .create()

      val localClient = new BspForwardClient(Some(launcher.getRemoteProxy))
      val userPaths = UserPaths.fromAppDirs

      val bloopRifleConfig = SetupBloopRifle(
        JavaCmd.javacommand,
        buildPaths,
        userPaths,
        CoursierResolver(logger, downloadSources = false, userPaths),
        bloopBspProtocol = Some("local")
      )

      val bloopRifleLogger = new BloopLogger(logger)

      val workspaceDir = buildPaths.bleepBloopDir.getParent

      var bloopServer: BloopServer = null
      try {
        bloopServer = BloopServer.buildServer(
          config = bloopRifleConfig,
          clientName = "bleep",
          clientVersion = constants.version,
          workspace = workspaceDir,
          classesDir = workspaceDir / "unused-classes-dir",
          buildClient = localClient,
          threads = threads.buildThreads,
          logger = bloopRifleLogger
        )

        bspBloopServer.bloopServer = bloopServer.server

        // run on `workspaceBuildTargets` later for the side-effect of updating build
        bspBloopServer.ensureBloopUpToDate = () =>
          ProjectSelection.load(buildPaths).flatMap { maybeSelectedProjectGlobs =>
            val bspRewrites: List[Rewrite] = List(
              List(rewrites.semanticDb(buildPaths)),
              maybeSelectedProjectGlobs match {
                case Some(selectedProjectGlobs) => List(rewrites.keepSelectedProjects(selectedProjectGlobs))
                case None                       => Nil
              }
            ).flatten

            bootstrap.from(logger, buildPaths, bspRewrites).map(_ => ())
          }

        logger.info {
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
}
