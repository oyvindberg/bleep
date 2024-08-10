package bleep
package commands

import bleep.internal.{traverseish, TransitiveProjects}
import bloop.rifle.BuildServer

case class SourceGen(watch: Boolean, projectNames: Array[model.CrossProjectName]) extends BleepCommandRemote(watch) {
  override def watchableProjects(started: Started): TransitiveProjects = {
    val scriptProjects = for {
      projectName <- projectNames
      p = started.build.explodedProjects(projectName)
      sourceGen <- p.sourcegen.values.iterator
      scriptProject = sourceGen match {
        case model.ScriptDef.Main(scriptProject, _, _) => scriptProject
      }
    } yield scriptProject
    TransitiveProjects(started.build, scriptProjects)
  }

  override def runWithServer(started: Started, bloop: BuildServer): Either[BleepException, Unit] = {
    val byScript: Map[model.ScriptDef, Array[model.CrossProjectName]] =
      projectNames
        .flatMap(projectName => started.build.explodedProjects(projectName).sourcegen.values.map(script => (script, projectName)))
        .groupMap { case (s, _) => s } { case (_, pn) => pn }

    traverseish.runAll(byScript) { case (script, projectNames) =>
      val args = projectNames.toList.flatMap(pn => List("--project", pn.value))
      Script.run(started, bloop, List(script), args = args, watch = false)
    }
  }
}
