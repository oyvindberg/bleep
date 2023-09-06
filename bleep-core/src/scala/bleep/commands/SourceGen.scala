package bleep
package commands

import bleep.internal.{traverseish, TransitiveProjects}

import bloop.rifle.BuildServer

case class SourceGen(watch: Boolean, projects: Array[model.CrossProjectName]) extends BleepCommandRemote(watch) {
  override def watchableProjects(started: Started): TransitiveProjects = {
    val scriptProjects = for {
      projectName <- projects
      p = started.build.explodedProjects(projectName)
      sourceGen <- p.sourcegen.values.iterator
      scriptProject = sourceGen match {
        case model.ScriptDef.Main(scriptProject, _, _) => scriptProject
      }
    } yield scriptProject
    TransitiveProjects(started.build, scriptProjects)
  }

  override def runWithServer(started: Started, bloop: BuildServer): Either[BleepException, Unit] =
    traverseish.runAll(projects) { projectName =>
      val scripts = started.build.explodedProjects(projectName).sourcegen.values.toList
      Script.run(started, bloop, scripts, args = List("--project", projectName.value), watch = false)
    }
}
