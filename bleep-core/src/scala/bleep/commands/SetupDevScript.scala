package bleep
package commands

import bleep.internal.{jvmRunCommand, FileUtils}

class SetupDevScript(started: Started, project: model.CrossProjectName, overrideMainClass: Option[String]) extends BleepCommand {
  private val safeName = project.value.replace('/', '-')

  override def run(): Either[BleepException, Unit] =
    OsArch.current.os match {
      case model.Os.Windows =>
        val cmd = jvmRunCommand(started.resolvedProject(project), started.resolvedJvm, project, overrideMainClass, List("%*")).orThrow.mkString(" ")
        val file =
          s"""@echo off
             |$cmd
             |""".stripMargin
        val scriptFile = started.buildPaths.buildDir / s"$safeName.bat"
        FileUtils.writeString(started.logger, None, scriptFile, file)
        scriptFile.toFile.setExecutable(true)
        Right(())
      case _ =>
        val cmd = jvmRunCommand(started.resolvedProject(project), started.resolvedJvm, project, overrideMainClass, List("$@")).orThrow.mkString(" ")

        val file =
          s"""#!/bin/sh
             |$cmd
             |""".stripMargin

        val scriptFile = started.buildPaths.buildDir / s"$safeName.sh"
        FileUtils.writeString(started.logger, None, scriptFile, file)
        scriptFile.toFile.setExecutable(true)
        Right(())
    }
}
