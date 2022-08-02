package bleep
package commands

import bleep.BleepException
import bleep.bsp.BspCommandFailed
import ch.epfl.scala.bsp4j
import ch.epfl.scala.bsp4j.{RunParams, ScalaMainClass, ScalaMainClassesParams, ScalaMainClassesResult}
import org.eclipse.lsp4j.jsonrpc.messages.ResponseError

import java.util
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
      maybeOverriddenMain.orElse(started.build.projects(project).platform.flatMap(_.mainClass))

    val maybeMain: Either[BleepException, String] =
      maybeSpecifiedMain match {
        case Some(mainClass) => Right(mainClass)
        case None =>
          started.logger.info("No main class specified in build or command line. discovering...")
          discoverMain(bloop)
      }

    maybeMain.flatMap { main =>
      val params = new RunParams(buildTarget(started.buildPaths, project))
      val mainClass = new ScalaMainClass(main, args.asJava, List(s"-Duser.dir=${started.prebootstrapped.buildPaths.cwd}").asJava)
      val envs = sys.env.map { case (k, v) => s"$k=$v" }.toList.sorted.asJava
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

  def discoverMain(bloop: BloopServer): Either[BleepException, String] = {
    val req = new ScalaMainClassesParams(util.List.of[bsp4j.BuildTargetIdentifier](buildTarget(started.buildPaths, project)))
    started.logger.debug(req.toString)

    val res: ScalaMainClassesResult =
      bloop.server.buildTargetScalaMainClasses(req).get()

    started.logger.debug(res.toString)

    res.getItems.asScala.flatMap(_.getClasses.asScala).map(_.getClassName).toList match {
      case Nil       => Left(Run.NoMain())
      case List(one) => Right(one)
      case many      => Left(Run.AmbiguousMain(many))
    }
  }
}

object Run {
  case class AmbiguousMain(mainClasses: Seq[String])
      extends BleepException(
        s"Discovered more than one main class, so you need to specify which one you want with `--class ...`. ${mainClasses.map(fansi.Color.Magenta(_)).mkString("\n", "\n, ", "\n")}"
      )

  case class NoMain() extends BleepException(s"No main class found. Specify which one you want with `--class ...`")
}
