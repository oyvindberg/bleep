package bleep

import cats.effect.{ExitCode, IO}
import com.monovore.decline._
import com.monovore.decline.effect.CommandIOApp

object Main
    extends CommandIOApp(
      name = "bleep",
      header = "Bleep for bloop!"
    ) {

  override def main: Opts[IO[ExitCode]] =
    List(
      Opts.subcommand("compile", "compile projects")(Opts(commands.Compile)),
      Opts.subcommand("test", "test projects")(Opts(commands.Test)),
      Opts.subcommand("script", "run script")(Opts.argument[commands.Script]()),
      Opts.subcommand("import", "import existing build from files in .bloop")(Opts(commands.Import)),
      Opts.subcommand("bsp", "bsp integration")(Opts(commands.Bsp)),
      Opts.subcommand("setup-ide", "generate ./bsp/bleep.json so IDEs can import build")(Opts(commands.SetupIde)),
      Opts.subcommand("clean", "clean")(Opts(commands.Clean))
    ).reduce[Opts[BleepCommand]](_.orElse(_)).map(_.run())
}
