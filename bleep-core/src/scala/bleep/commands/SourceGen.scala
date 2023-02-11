package bleep
package commands

import bleep.internal.traverseish

import scala.build.bloop.BloopServer

case class SourceGen(watch: Boolean, projects: Array[model.CrossProjectName]) extends BleepCommandRemote(watch) {
  override def watchableProjects(started: Started): Array[model.CrossProjectName] =
    projects

  override def runWithServer(started: Started, bloop: BloopServer): Either[BleepException, Unit] =
    traverseish.runAll(projects) { projectName =>
      val scripts = started.build.explodedProjects(projectName).sourcegen.values.toList
      Script.run(started, bloop, scripts, args = List("--project", projectName.value), watch = false)
    }
}
