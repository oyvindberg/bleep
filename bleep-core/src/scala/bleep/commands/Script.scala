package bleep
package commands

import bleep.BleepException
import cats.syntax.traverse._

import scala.build.bloop.BloopServer

case class Script(name: model.ScriptName, args: List[String], watch: Boolean) extends BleepCommandRemote(watch) {
  override def watchableProjects(started: Started): Array[model.CrossProjectName] =
    started.build
      .scripts(name)
      .values
      .map {
        case model.ScriptDef.Main(project, _) => project
        case model.ScriptDef.Shell(_, _)      => throw new BleepException.Text("cannot `--watch shell` scripts for now")
      }
      .distinct
      .toArray

  override def runWithServer(started: Started, bloop: BloopServer): Either[BleepException, Unit] =
    started.build
      .scripts(name)
      .values
      .traverse {
        case model.ScriptDef.Main(project, main) =>
          Run(project, Some(main), args = args, raw = false, watch = watch).runWithServer(started, bloop)
        case model.ScriptDef.Shell(command, overridesForOs) =>
          val bareCommand: String = overridesForOs.flatMap(_.get(OsArch.current.os)).orElse(command).getOrElse {
            throw new BleepException.Text(s"no command found for os ${OsArch.current.os}")
          }

          val cmd = OsArch.current.os match {
            case model.Os.Windows =>
              List("cmd.exe", "/C", bareCommand)
            case _ =>
              val quote = '"'.toString
              val quotedQuote = "\\\\"
              List("bash", "-c", bareCommand.replace(quote, quotedQuote))
          }
          try {
            cli(
              action = s"script ${name.value}",
              cwd = started.buildPaths.cwd,
              cmd = cmd,
              logger = started.logger,
              out = cli.Out.ViaLogger(started.logger),
              in = cli.In.Attach
            )
            Right(())
          } catch {
            case x: BleepException => Left(x)
          }
      }
      .map(_ => ())
}
