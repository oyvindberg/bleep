package bleep
package bsp

import bleep.internal.MyBloopRifleLogger
import ch.epfl.scala.bsp4j
import org.eclipse.lsp4j.jsonrpc

import java.io.{InputStream, OutputStream}
import java.nio.file.Path
import scala.build.bloop.BloopServer
import scala.build.blooprifle.BloopRifleConfig
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

final class BspImpl(
    logger: Logger,
    bloopRifleConfig: BloopRifleConfig,
    buildPath: Path,
    threads: BspThreads,
    in: InputStream,
    out: OutputStream
) {

  val bloopRifleLogger = new MyBloopRifleLogger(logger, true, true)
  var remoteServer: BloopServer = null
  var actualLocalServer: BspServer = null

  def classesRootDir(root: Path): Path =
    root / ".bleep" / "classes"

  def run(): Future[Unit] = {
    val localClient = new BspClient(None)

    remoteServer = BloopServer.buildServer(
      bloopRifleConfig,
      "scala-cli",
      Defaults.version,
      buildPath,
      classesRootDir(buildPath),
      localClient,
      threads.buildThreads.bloop,
      bloopRifleLogger
    )

    val localServer: bsp4j.BuildServer with bsp4j.ScalaBuildServer with bsp4j.JavaBuildServer =
      new BspServer(remoteServer.server)

    val launcher = new jsonrpc.Launcher.Builder[bsp4j.BuildClient]()
      .setExecutorService(threads.buildThreads.bloop.jsonrpc) // FIXME No
      .setInput(in)
      .setOutput(out)
      .setRemoteInterface(classOf[bsp4j.BuildClient])
      .setLocalService(localServer)
      .create()

    localClient.forwardToOpt = Some(launcher.getRemoteProxy)

    logger.info {
      val hasConsole = System.console() != null
      if (hasConsole)
        "Listening to incoming JSONRPC BSP requests, press Ctrl+D to exit."
      else
        "Listening to incoming JSONRPC BSP requests."
    }
    val f = launcher.startListening()

    val es = ExecutionContext.fromExecutorService(threads.buildThreads.bloop.jsonrpc)
    val futures = Seq(
      BspImpl.naiveJavaFutureToScalaFuture(f).map(_ => ())(es),
      actualLocalServer.initiateShutdown
    )
    Future.firstCompletedOf(futures)(es)
  }

  def shutdown(): Unit =
    if (remoteServer != null)
      remoteServer.shutdown()
}

object BspImpl {

  // from https://githubsp4j.com/com-lihaoyi/Ammonite/blob/7eb58c58ec8c252dc5bd1591b041fcae01cccf90/amm/interp/src/main/scala/ammonite/interp/script/AmmoniteBuildServer.scala#L550-L565
  private def naiveJavaFutureToScalaFuture[T](
      f: java.util.concurrent.Future[T]
  ): Future[T] = {
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
