package bleep
package commands

import bleep.internal.{jvmRunCommand, FileUtils}

class SetupDevScript(started: Started, project: model.CrossProjectName, overrideMainClass: Option[String]) extends BleepCommand {
  override def run(): Either[BleepException, Unit] =
    OsArch.current.os match {
      case model.Os.Windows =>
        val cmd = jvmRunCommand(started.bloopProject(project), started.resolvedJvm, project, overrideMainClass, List("%*")).orThrow.mkString(" ")
        val file =
          s"""@echo off
             |$cmd
             |""".stripMargin
        val scriptFile = started.buildPaths.buildDir / s"${project.value}.bat"
        FileUtils.writeString(started.logger, None, scriptFile, file)
        scriptFile.toFile.setExecutable(true)
        Right(())
      case _ =>
        val cmd = jvmRunCommand(started.bloopProject(project), started.resolvedJvm, project, overrideMainClass, List("$@")).orThrow.mkString(" ")

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
