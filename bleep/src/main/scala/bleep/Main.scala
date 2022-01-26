package bleep

import bleep.internal.Os
import bleep.logging.{LogLevel, Logger}
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

  def mainOpts(logger: Logger): Opts[BleepCommand] = {
    lazy val bootstrapped: Either[BuildException, Started] =
      bootstrap.from(logger, cwd)

    def forceStarted: Started = bootstrapped match {
      case Left(buildException) =>
        logger.error("couldn't initialize", buildException)
        sys.exit(1)
      case Right(started) => started
    }

    def projectCompletions(projects: Iterable[model.CrossProjectName]): Map[String, Iterable[model.CrossProjectName]] = {
      val crossNames: Map[String, Iterable[model.CrossProjectName]] =
        projects.map(projectName => projectName.value -> List(projectName)).toMap
      val projectNames: Map[String, Iterable[model.CrossProjectName]] =
        projects.groupBy { case model.CrossProjectName(name, _) => name.value }
      val crossIds: Map[String, Iterable[model.CrossProjectName]] =
        projects
          .groupBy { case model.CrossProjectName(_, crossId) => crossId }
          .collect { case (Some(crossId), names) => (crossId.value, names) }

      crossIds ++ projectNames ++ crossNames

    }
    def projectNameMap: Map[String, Iterable[model.CrossProjectName]] =
      bootstrapped match {
        case Left(_)        => Map.empty
        case Right(started) => projectCompletions(started.build.projects.keys)
      }

    def testProjectNameMap: Map[String, Iterable[model.CrossProjectName]] =
      bootstrapped match {
        case Left(_)        => Map.empty
        case Right(started) => projectCompletions(started.build.projects.filter { case (_, p) => !p.testFrameworks.isEmpty }.keys)
      }

    def projectNames: Opts[Option[List[model.CrossProjectName]]] =
      Opts.arguments("project name")(Argument.fromMap("project name", projectNameMap)).map(_.toList.flatten).orNone

    def testProjectNames: Opts[Option[List[model.CrossProjectName]]] =
      Opts.arguments("test project name")(Argument.fromMap("test project name", testProjectNameMap)).map(_.toList.flatten).orNone

    lazy val ret: Opts[BleepCommand] = List(
      List(
        Opts.subcommand("build", "rewrite build")(
          List(
            Opts.subcommand("reapply-templates", "apply templates again. will minimize ")(
              CommonOpts.opts.map(opts => commands.BuildReapplyTemplates(forceStarted, opts))
            )
          ).foldK
        ),
        Opts.subcommand("compile", "compile projects")(
          (CommonOpts.opts, projectNames).mapN { case (opts, projectNames) => commands.Compile(forceStarted, opts, projectNames) }
        ),
        Opts.subcommand("test", "test projects")(
          (CommonOpts.opts, testProjectNames).mapN { case (opts, projectNames) => commands.Test(forceStarted, opts, projectNames) }
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
        Opts.subcommand("patch", "Apply patch from standard-in or file")(
          (CommonOpts.opts, Opts.option[Path]("file", "patch file, defaults to std-in").orNone).mapN((opts, file) => commands.Patch(forceStarted, opts, file))
        ),
        Opts.subcommand("import", "import existing build from files in .bloop")(Opts(commands.Import(logger, Set.empty))),
        Opts.subcommand("_complete", "tab-completions")(
          (Opts.argument[String]("COMP_LINE"), Opts.argument[Int]("COMP_CWORD"), Opts.argument[Int]("COMP_POINT")).mapN {
            case (compLine, compCword, compPoint) =>
              new BleepCommand {
                override def run(): Unit = {
                  val completer = new Completer({
                    case "project name"      => projectNameMap.keys.toList
                    case "test project name" => testProjectNameMap.keys.toList
                    case _                   => Nil
                  })
                  completer.bash(compLine, compCword, compPoint)(ret).foreach(c => println(c.value))
                }
              }
          }
        )
      ),
      bootstrapped match {
        case Left(_) => Nil
        case Right(started) =>
          started.build.scripts.map { case (scriptName, scriptDefs) =>
            Opts.subcommand(scriptName.value, s"run script ${scriptName.value}")(
              stringArgs.map(args => commands.Script(forceStarted, scriptName, scriptDefs, args))
            )
          }
      }
    ).flatten.foldK

    ret
  }

  def main(args: Array[String]): Unit = {
    // don't produce garbage output when completing
    // also need to refactor here. there is a circular dependency here where bootstrap needs parsed params, parsing params needs bootstrap
    val logger =
      if (args.headOption.contains("_complete")) Logger.DevNull
      else if (args.contains("--no-color") || System.console() == null)
        logging.stdout(LogPatterns.logFile).filter(LogLevel.info)
      else logging.stdout(LogPatterns.interface(Instant.now, None)).filter(LogLevel.info)

    Command("bleep", "Bleeping fast build!")(mainOpts(logger)).parse(args.toIndexedSeq, sys.env) match {
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
