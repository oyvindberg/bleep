package bleep

import bleep.internal.Os
import bleep.logging.{Logger, Pattern}
import cats.data.NonEmptyList
import cats.syntax.apply._
import com.monovore.decline._

import java.io.PrintStream
import java.nio.file.Path
import scala.util.{Failure, Success, Try}

object Main {
  val logger: Logger.Aux[PrintStream] =
    logging.appendable(System.out, Pattern.default)

  val cwd: Path =
    Os.cwd

  val stringArgs: Opts[List[String]] =
    Opts.arguments[String]().orNone.map(args => args.fold(List.empty[String])(_.toList))

  val mainOpts: List[Opts[BleepCommand]] = {
    bootstrap.from(logger, cwd) match {
      case Left(buildException) =>
        List[Opts[BleepCommand]](
          Opts.subcommand("setup-ide", "generate ./bsp/bleep.json so IDEs can import build")(
            Opts(commands.SetupIde(BuildPaths(cwd / "bleep.json"), logger))
          ),
          Opts.subcommand("import", "import existing build from files in .bloop")(Opts(commands.Import(logger))),
          Opts(() => logger.error("couldn't initialize bleep", buildException))
        )

      case Right(started) =>
        val projectNames: Opts[Option[NonEmptyList[model.ProjectName]]] = {
          val byName = started.build.projects.keys.map(projectName => projectName.value -> projectName).toMap
          Opts.arguments[model.ProjectName]("project name")(Argument.fromMap("project name", byName)).orNone
        }

        List(
          List(
            Opts.subcommand("compile", "compile projects")(
              (CommonOpts.opts, projectNames).mapN { case (opts, projectNames) => commands.Compile(started, opts, projectNames) }
            ),
            Opts.subcommand("test", "test projects")(
              (CommonOpts.opts, projectNames).mapN { case (opts, projectNames) => commands.Test(started, opts, projectNames) }
            ),
            Opts.subcommand("bsp", "bsp integration")(
              CommonOpts.opts.map(opts => commands.Bsp(opts, started))
            ),
            Opts.subcommand("clean", "clean")(
              (CommonOpts.opts, projectNames).mapN { case (opts, projectNames) => commands.Clean(started, opts, projectNames) }
            ),
            Opts.subcommand("setup-ide", "generate ./bsp/bleep.json so IDEs can import build")(
              Opts(commands.SetupIde(started.buildPaths, logger))
            ),
            Opts.subcommand("import", "import existing build from files in .bloop")(Opts(commands.Import(logger)))
          ),
          started.build.scripts.getOrElse(Map.empty).map { case (scriptName, scriptDefs) =>
            Opts.subcommand(scriptName.value, s"run script ${scriptName.value}")(
              stringArgs.map(args => commands.Script(started, scriptName, scriptDefs, args))
            )
          }
        ).flatten
    }
  }
  def main(args: Array[String]): Unit =
    Command("bleep", "Bleeping fast build!")(mainOpts.reduce(_.orElse(_))).parse(args.toIndexedSeq, sys.env) match {
      case Left(help) => System.err.println(help)
      case Right(cmd) =>
        Try(cmd.run()) match {
          case Failure(unexpected) =>
            logger.error("Error while running command", unexpected)
          case Success(_) =>
            ()
        }
    }
}
