package bleep
package commands

import cats.effect.{ExitCode, IO}

case object Test extends BleepCommand {
  override def run(): IO[ExitCode] =
    runWithEnv { started =>
      val projects = started.build.projects.keys.map(_.value).mkString(" ")
      IO(ExitCode(cli(s"bloop test $projects")(started.buildPaths.buildDir)))
    }
}
