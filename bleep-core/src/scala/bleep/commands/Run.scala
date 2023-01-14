package bleep
package commands

import bleep.BleepException
import bleep.bsp.BspCommandFailed
import bleep.internal.jvmRunCommand
import bleep.logging.jsonEvents
import ch.epfl.scala.bsp4j
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException
import org.eclipse.lsp4j.jsonrpc.messages.ResponseError

import java.util.concurrent.ExecutionException
import scala.annotation.tailrec
import scala.build.bloop.BloopServer
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

/** @param raw
  *   use raw stdin and stdout and avoid the logger
  */
case class Run(
    project: model.CrossProjectName,
    maybeOverriddenMain: Option[String],
    args: List[String],
    raw: Boolean,
    watch: Boolean
) extends BleepCommandRemote(watch) {
  override def watchableProjects(started: Started): Array[model.CrossProjectName] = Array(project)

  override def runWithServer(started: Started, bloop: BloopServer): Either[BleepException, Unit] = {
    val maybeSpecifiedMain: Option[String] =
      maybeOverriddenMain.orElse(started.build.explodedProjects(project).platform.flatMap(_.mainClass))

    val maybeMain: Either[BleepException, String] =
      maybeSpecifiedMain match {
        case Some(mainClass) => Right(mainClass)
        case None =>
          started.logger.info("No main class specified in build or command line. discovering...")
          discoverMain(started.logger, bloop, buildTarget(started.buildPaths, project))
      }

    maybeMain.flatMap { main =>
      started.build.explodedProjects(project).platform.flatMap(_.name) match {
        // we could definitely run js/native projects in "raw" mode as well, it just needs to be implemented
        case Some(model.PlatformId.Jvm) if raw => rawRun(started, bloop, main)
        case _                                 => bspRun(started, bloop, main)
      }
    }
  }

  def rawRun(started: Started, bloop: BloopServer, main: String): Either[BleepException, Unit] =
    Compile(watch = false, Array(project)).runWithServer(started, bloop).map { case () =>
      cli(
        "run",
        started.pre.buildPaths.cwd,
        jvmRunCommand(started, project, Some(main), args).orThrow,
        logger = started.logger,
        out = cli.Out.Raw,
        in = cli.In.Attach,
        env = sys.env.toList
      )
      ()
    }

  def bspRun(started: Started, bloop: BloopServer, main: String): Either[BspCommandFailed, Unit] = {
    val params = new bsp4j.RunParams(buildTarget(started.buildPaths, project))
    val mainClass = new bsp4j.ScalaMainClass(main, args.asJava, List(s"-Duser.dir=${started.pre.buildPaths.cwd}").asJava)
    val envs = sys.env.updated(jsonEvents.CallerProcessAcceptsJsonEvents, "true").map { case (k, v) => s"$k=$v" }.toList.sorted.asJava
    mainClass.setEnvironmentVariables(envs)
    params.setData(mainClass)
    params.setDataKind("scala-main-class")
    started.logger.debug(params.toString)

    def failed(reason: BspCommandFailed.Reason) =
      Left(new BspCommandFailed("Run", Array(project), reason))

    Try(bloop.server.buildTargetRun(params).get().getStatusCode) match {
      case Success(bsp4j.StatusCode.OK) => Right(started.logger.info("Run succeeded"))
      case Success(errorCode)           => failed(BspCommandFailed.StatusCode(errorCode))
      case Failure(exception) =>
        @tailrec
        def findResponseError(th: Throwable): Option[ResponseError] =
          th match {
            case x: ExecutionException =>
              findResponseError(x.getCause)
            case x: ResponseErrorException =>
              Option(x.getResponseError)
            case _ => None
          }

        findResponseError(exception) match {
          case Some(responseError) => failed(BspCommandFailed.FoundResponseError(responseError))
          case None                => failed(BspCommandFailed.FailedWithException(exception))
        }
    }
  }
}
