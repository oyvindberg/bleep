package bleep
package commands

import bleep.internal.TransitiveProjects
import bleep.packaging.dist
import bloop.rifle.BuildServer

import java.nio.file.Path

object Dist {
  case class Options(
      project: model.CrossProjectName,
      overrideMain: Option[String],
      overridePath: Option[Path]
  )
}

case class Dist(started: Started, watch: Boolean, options: Dist.Options) extends BleepCommandRemote(watch) {
  override def watchableProjects(started: Started): TransitiveProjects =
    TransitiveProjects(started.build, Array(options.project))

  override def runWithServer(started: Started, bloop: BuildServer): Either[BleepException, Unit] =
    for {
      _ <- Compile(watch = false, Array(options.project)).runWithServer(started, bloop)
      mainClass <- options.overrideMain match {
        case Some(x) => Right(x)
        case None =>
          started.build.explodedProjects(options.project).platform.flatMap(_.mainClass) match {
            case Some(x) => Right(x)
            case None    => discoverMain(started.logger, bloop, BleepCommandRemote.buildTarget(started.buildPaths, options.project))
          }
      }
    } yield {
      val program = dist.Program(options.project.name.value, mainClass)
      dist(started, options.project, List(program), overridePath = options.overridePath)
    }
}
