package bleep
package commands

import bleep.BleepException
import bleep.bsp.BspCommandFailed
import bleep.logging.jsonEvents
import ch.epfl.scala.bsp4j
import org.eclipse.lsp4j.jsonrpc.messages.ResponseError

import scala.build.bloop.BloopServer
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

case class Run(
    started: Started,
    project: model.CrossProjectName,
    maybeOverriddenMain: Option[String],
    args: List[String]
) extends BleepCommandRemote(started) {
  override def runWithServer(bloop: BloopServer): Either[BleepException, Unit] = {
    val maybeSpecifiedMain: Option[String] =
      maybeOverriddenMain.orElse(started.build.explodedProjects(project).platform.flatMap(_.mainClass))

    val maybeMain: Either[BleepException, String] =
      maybeSpecifiedMain match {
        case Some(mainClass) => Right(mainClass)
        case None =>
          started.logger.info("No main class specified in build or command line. discovering...")
          discoverMain(bloop, project)
      }

    maybeMain.flatMap { main =>
      val params = new bsp4j.RunParams(buildTarget(started.buildPaths, project))
      val mainClass = new bsp4j.ScalaMainClass(main, args.asJava, List(s"-Duser.dir=${started.prebootstrapped.buildPaths.cwd}").asJava)
      val envs = sys.env.updated(jsonEvents.CallerProcessAcceptsJsonEvents, "true").map { case (k, v) => s"$k=$v" }.toList.sorted.asJava
      mainClass.setEnvironmentVariables(envs)
      params.setData(mainClass)
      params.setDataKind("scala-main-class")
      started.logger.debug(params.toString)

      def failed(reason: BspCommandFailed.Reason) =
        Left(new BspCommandFailed("Run", List(project), reason))

      Try(bloop.server.buildTargetRun(params).get().getStatusCode) match {
        case Success(bsp4j.StatusCode.OK) => Right(started.logger.info("Run succeeded"))
        case Success(errorCode)           => failed(BspCommandFailed.StatusCode(errorCode))
        case Failure(exception) =>
          def findResponseError(th: Throwable): Option[ResponseError] =
            th match {
              case x: java.util.concurrent.ExecutionException =>
                findResponseError(x.getCause)
              case x: org.eclipse.lsp4j.jsonrpc.ResponseErrorException =>
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
}
