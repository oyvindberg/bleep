package bleep
package commands

import bleep.packaging.dist

import java.nio.file.Path
import scala.build.bloop.BloopServer

object Dist {
  case class Options(
      project: model.CrossProjectName,
      overrideMain: Option[String],
      overridePath: Option[Path]
  )
}

case class Dist(started: Started, options: Dist.Options) extends BleepCommandRemote(started) {
  override def runWithServer(bloop: BloopServer): Either[BleepException, Unit] =
    for {
      _ <- Compile(started, List(options.project)).runWithServer(bloop)
      mainClass <- options.overrideMain match {
        case Some(x) => Right(x)
        case None =>
          started.build.explodedProjects(options.project).platform.flatMap(_.mainClass) match {
            case Some(x) => Right(x)
            case None    => discoverMain(bloop, options.project)
          }
      }
    } yield {
      val program = dist.Program(options.project.name.value, mainClass)
      dist(started, options.project, List(program), overridePath = options.overridePath)
    }
}
