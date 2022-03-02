package bleep
package commands

import ch.epfl.scala.bsp4j
import ch.epfl.scala.bsp4j.{RunParams, ScalaMainClass, ScalaMainClassesParams}

import java.util
import scala.build.bloop.BloopServer
import scala.jdk.CollectionConverters._

case class Run(
    started: Started,
    project: model.CrossProjectName,
    maybeOverridenMain: Option[String],
    args: List[String]
) extends BleepCommandRemote {
  override def runWithServer(bloop: BloopServer): Unit = {
    val params = new RunParams(buildTarget(started.buildPaths, project))

    maybeOverridenMain
      .orElse(started.build.projects(project).platform.flatMap(_.jvmMainClass))
      .orElse {
        val mainClassesRequest = new ScalaMainClassesParams(util.List.of(buildTarget(started.buildPaths, project)))
        started.logger.debug(mainClassesRequest.toString)

        val mainClassesResult: bsp4j.ScalaMainClassesResult =
          bloop.server.buildTargetScalaMainClasses(mainClassesRequest).get()
        started.logger.debug(mainClassesResult.toString)

        val all = mainClassesResult.getItems.asScala.flatMap(_.getClasses.asScala).map(_.getClassName)
        started.logger.warn(s"Discovered these main classes. ${all.map(fansi.Color.Magenta(_)).mkString(", ")}")
        all.headOption
      }
      .foreach { main =>
        val mainClass = new ScalaMainClass(main, args.asJava, util.List.of())
        mainClass.setEnvironmentVariables(util.List.of())
        params.setData(mainClass)
        params.setDataKind("scala-main-class")
      }

    started.logger.debug(params.toString)

    bloop.server.buildTargetRun(params).get().getStatusCode match {
      case bsp4j.StatusCode.OK        => started.logger.info("Run succeeded")
      case bsp4j.StatusCode.ERROR     => started.logger.warn("Run failed")
      case bsp4j.StatusCode.CANCELLED => started.logger.info("Run cancelled")
    }
  }
}
