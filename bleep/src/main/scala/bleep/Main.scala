package bleep

import bleep.BleepCommands._
import cats.data.NonEmptyList
import cats.effect.{ExitCode, IO}
import com.monovore.decline._
import com.monovore.decline.effect.CommandIOApp

object Main
    extends CommandIOApp(
      name = "bleep",
      header = "Bleep for bloop!"
    ) {
  override def main: Opts[IO[ExitCode]] =
    NonEmptyList.of(compile, test, script).reduce[Opts[BleepCommands]](_ orElse _).map(_.run())

  private val compile = Opts.subcommand("compile", "compile all projects")(Opts(Compile))
  private val test = Opts.subcommand("test", "test all projects")(Opts(Test))
  private val script = Opts.subcommand("script", "run script")(Opts.argument[Script]())
}
