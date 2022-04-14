package bleep
package commands

import ch.epfl.scala.bsp4j
import ch.epfl.scala.bsp4j.{RunParams, ScalaMainClass, ScalaMainClassesParams, ScalaMainClassesResult}

import java.util
import scala.build.bloop.BloopServer
import scala.jdk.CollectionConverters._

case class Run(
    started: Started,
    project: model.CrossProjectName,
    maybeOverriddenMain: Option[String],
    args: List[String]
) extends BleepCommandRemote(started) {
  override def runWithServer(bloop: BloopServer): Either[BuildException, Unit] = {
    val maybeSpecifiedMain: Option[String] =
      maybeOverriddenMain.orElse(started.build.projects(project).platform.flatMap(_.mainClass))

    val maybeMain: Either[BuildException, String] =
      maybeSpecifiedMain match {
        case Some(mainClass) => Right(mainClass)
        case None =>
          started.logger.info("No main class specified in build or command line. discovering...")
          discoverMain(bloop)
      }

    maybeMain.flatMap { main =>
      val params = new RunParams(buildTarget(started.buildPaths, project))
      val mainClass = new ScalaMainClass(main, args.asJava, util.List.of())
      mainClass.setEnvironmentVariables(util.List.of())
      params.setData(mainClass)
      params.setDataKind("scala-main-class")
      started.logger.debug(params.toString)

      bloop.server.buildTargetRun(params).get().getStatusCode match {
        case bsp4j.StatusCode.OK => Right(started.logger.info("Run succeeded"))
        case other               => Left(new BspCommandFailed("Run", List(project), other))
      }
    }
  }

  def discoverMain(bloop: BloopServer): Either[BuildException, String] = {
    val req = new ScalaMainClassesParams(util.List.of(buildTarget(started.buildPaths, project)))
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
      extends BuildException(
        s"Discovered more than one main class, so you need to specify which one you want with `--class ...`. ${mainClasses.map(fansi.Color.Magenta(_)).mkString("\n", "\n, ", "\n")}"
      )

  case class NoMain() extends BuildException(s"No main class found. Specify which one you want with `--class ...`")
}
