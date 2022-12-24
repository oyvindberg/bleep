package bleep
package commands

import bleep.internal.FileUtils
import bleep.internal.jvmRunCommand
import bleep.model.Os

class SetupDevScript(started: Started, project: model.CrossProjectName, overrideMainClass: Option[String]) extends BleepCommand {
  override def run(): Either[BleepException, Unit] =
    OsArch.current.os match {
      case Os.Windows => throw new BleepException.Text(s"Not implemented for windows")
      case _ =>
        val cmd = jvmRunCommand(started.bloopProjects(project), started.resolvedJvm, project, overrideMainClass, List("$@")).orThrow.mkString(" ")

        val file =
          s"""#!/bin/sh
             |$cmd
             |""".stripMargin

        val scriptFile = started.buildPaths.buildDir / s"${project.value}.sh"
        FileUtils.writeString(started.logger, None, scriptFile, file)
        scriptFile.toFile.setExecutable(true)
        Right(())
    }
}
