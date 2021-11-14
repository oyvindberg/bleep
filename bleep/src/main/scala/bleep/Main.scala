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
    NonEmptyList.of(compile, test, script).reduce(_ orElse _).map(_.run())

  private val compile = Opts.subcommand("compile", "compile all project")(Opts(Compile))
  private val test = Opts.subcommand("test", "compile all project")(Opts(Compile))
  private val script = Opts.subcommand("script", "run script")(Opts.argument[Script]())
}
