package bleep

import bleep.internal.Os
import bleep.logging.{LogLevel, Logger}
import cats.data.NonEmptyList
import cats.syntax.apply._
import cats.syntax.foldable._
import com.monovore.decline._

import java.nio.file.Path
import java.time.Instant
import scala.util.{Failure, Success, Try}

object Main {
  val cwd: Path =
    Os.cwd

  val stringArgs: Opts[List[String]] =
    Opts.arguments[String]().orNone.map(args => args.fold(List.empty[String])(_.toList))

  def mainOpts(logger: Logger): List[Opts[BleepCommand]] = {
    lazy val bootstrapped: Either[BuildException, Started] =
      bootstrap.from(logger, cwd)

    def forceStarted: Started = bootstrapped match {
      case Left(buildException) =>
        logger.error("couldn't initialize", buildException)
        sys.exit(1)
      case Right(started) => started
    }

    val projectNameMap: Map[String, model.ProjectName] =
      bootstrapped match {
        case Left(_)        => Map.empty
        case Right(started) => started.build.projects.keys.map(projectName => projectName.value -> projectName).toMap
      }

    val projectNames: Opts[Option[NonEmptyList[model.ProjectName]]] =
      Opts.arguments("project name")(Argument.fromMap("project name", projectNameMap)).orNone

    lazy val ret: List[Opts[BleepCommand]] = List(
      List(
        Opts.subcommand("compile", "compile projects")(
          (CommonOpts.opts, projectNames).mapN { case (opts, projectNames) => commands.Compile(forceStarted, opts, projectNames) }
        ),
        Opts.subcommand("test", "test projects")(
          (CommonOpts.opts, projectNames).mapN { case (opts, projectNames) => commands.Test(forceStarted, opts, projectNames) }
        ),
        Opts.subcommand("bsp", "bsp integration")(
          CommonOpts.opts.map(opts => commands.Bsp(opts, forceStarted))
        ),
        Opts.subcommand("clean", "clean")(
          (CommonOpts.opts, projectNames).mapN { case (opts, projectNames) => commands.Clean(forceStarted, opts, projectNames) }
        ),
        Opts.subcommand("setup-ide", "generate ./bsp/bleep.json so IDEs can import build")(
          bootstrapped match {
            case Left(_)        => Opts(commands.SetupIde(BuildPaths(cwd / "bleep.json"), logger))
            case Right(started) => Opts(commands.SetupIde(started.buildPaths, logger))
          }
        ),
        Opts.subcommand("import", "import existing build from files in .bloop")(Opts(commands.Import(logger))),
        Opts.subcommand("_complete", "tab-completions")(
          (Opts.argument[String]("COMP_LINE"), Opts.argument[Int]("COMP_CWORD"), Opts.argument[Int]("COMP_POINT")).mapN {
            case (compLine, compCword, compPoint) =>
              new BleepCommand {
                override def run(): Unit = {
                  val completer = new Completer({
                    case "project name" => projectNameMap.keys.toList
                    case _              => Nil
                  })
                  completer.bash(compLine, compCword, compPoint)(ret.foldK).foreach(println)
                }
              }
          }
        )
      ),
      bootstrapped match {
        case Left(_) => Nil
        case Right(started) =>
          started.build.scripts.getOrElse(Map.empty).map { case (scriptName, scriptDefs) =>
            Opts.subcommand(scriptName.value, s"run script ${scriptName.value}")(
              stringArgs.map(args => commands.Script(forceStarted, scriptName, scriptDefs, args))
            )
          }
      }
    ).flatten

    ret
  }

  def main(args: Array[String]): Unit = {
    // don't produce garbage output when completing
    val logger =
      if (args.headOption.contains("_complete")) Logger.DevNull
      else logging.stdout(LogPatterns.interface(Instant.now, None)).filter(LogLevel.info)

    Command("bleep", "Bleeping fast build!")(mainOpts(logger).foldK).parse(args.toIndexedSeq, sys.env) match {
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
}
