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
    List(compile, test, script, `import`, bsp).reduce[Opts[BleepCommands]](_.orElse(_)).map(_.run())

  private val compile = Opts.subcommand("compile", "compile all projects")(Opts(BleepCommands.Compile))
  private val test = Opts.subcommand("test", "test all projects")(Opts(BleepCommands.Compile))
  private val script = Opts.subcommand("script", "run script")(Opts.argument[BleepCommands.Script]())
  private val `import` = Opts.subcommand("import", "import existing build from files in .bloop")(Opts(BleepCommands.Import))
  private val bsp = Opts.subcommand("bsp", "bsp integration")(Opts(BleepCommands.Bsp))
}
