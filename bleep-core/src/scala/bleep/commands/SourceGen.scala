package bleep
package commands

import bleep.internal.{traverseish, TransitiveProjects}

case class SourceGen(watch: Boolean, projectNames: Array[model.CrossProjectName]) extends BleepBuildCommand {
  override def run(started: Started): Either[BleepException, Unit] =
    if (watch) WatchMode.run(started, watchableProjects)(runOnce)
    else runOnce(started)

  /** What `--watch` wakes up on: the script projects, and the projects that asked for them.
    *
    * The consumers are in the set because that is where `sourceGlobs` is declared, and those directories are now real inputs to the staleness check. Seeding
    * only the script projects meant a schema directory declared on the consumer could change without waking anything — the same half-wired state the field had
    * everywhere else.
    */
  private def watchableProjects(started: Started): TransitiveProjects = {
    val scriptProjects = for {
      projectName <- projectNames
      p = started.build.explodedProjects(projectName)
      sourceGen <- p.sourcegen.values.iterator
      scriptProject = sourceGen match {
        case model.ScriptDef.Main(scriptProject, _, _) => scriptProject
      }
    } yield scriptProject
    TransitiveProjects(started.build, projectNames ++ scriptProjects)
  }

  private def runOnce(started: Started): Either[BleepException, Unit] = {
    val byScript: Map[model.ScriptDef, Array[model.CrossProjectName]] =
      projectNames
        .flatMap(projectName => started.build.explodedProjects(projectName).sourcegen.values.map(script => (script, projectName)))
        .groupMap { case (s, _) => s } { case (_, pn) => pn }

    traverseish.runAll(byScript) { case (script, projectNames) =>
      val args = projectNames.toList.flatMap(pn => List("--project", pn.value))
      Script.run(started, List(script), args = args, watch = false)
    }
  }
}
