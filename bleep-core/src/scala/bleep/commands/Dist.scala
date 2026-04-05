package bleep
package commands

import bleep.internal.TransitiveProjects
import bleep.packaging.dist

import java.nio.file.Path

object Dist {
  case class Options(
      project: model.CrossProjectName,
      overrideMain: Option[String],
      overridePath: Option[Path]
  )
}

case class Dist(watch: Boolean, options: Dist.Options, buildOpts: CommonBuildOpts) extends BleepBuildCommand {
  override def run(started: Started): Either[BleepException, Unit] =
    if (watch) WatchMode.run(started, s => TransitiveProjects(s.build, Array(options.project)))(runOnce)
    else runOnce(started)

  private def runOnce(started: Started): Either[BleepException, Unit] =
    for {
      _ <- ReactiveBsp
        .compile(
          watch = false,
          projects = Array(options.project),
          displayMode = buildOpts.displayMode,
          flamegraph = buildOpts.flamegraph,
          cancel = buildOpts.cancel
        )
        .run(started)
      mainClass <- options.overrideMain match {
        case Some(x) => Right(x)
        case None    =>
          started.build.explodedProjects(options.project).platform.flatMap(_.mainClass) match {
            case Some(x) => Right(x)
            case None    =>
              BspQuery.withServer(started) { server =>
                discoverMain(started.logger, server, BspQuery.buildTarget(started.buildPaths, options.project))
              }
          }
      }
    } yield {
      val program = dist.Program(options.project.name.value, mainClass)
      dist(started, options.project, List(program), overridePath = options.overridePath)
    }
}
