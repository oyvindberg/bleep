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
      scriptProject <- sourceGen match {
        case model.ScriptDef.Main(scriptProject, _, _, inputs, _) => scriptProject :: inputs.values.toList
      }
    } yield scriptProject
    TransitiveProjects(started.build, projectNames ++ scriptProjects)
  }

  private def runOnce(started: Started): Either[BleepException, Unit] =
    traverseish.runAll(SourceGen.plan(started.build, projectNames)) { invocation =>
      Script.run(started, List(invocation.script), args = invocation.args, watch = false)
    }
}

object SourceGen {

  /** One sourcegen script to run, and the projects it generates for. A script shared by several projects runs once, told about all of them. */
  case class Invocation(script: model.ScriptDef.Main, forProjects: List[model.CrossProjectName]) {
    def args: List[String] = forProjects.flatMap(pn => List("--project", pn.value))
  }

  /** Which scripts `bleep sourcegen` runs for `projectNames`, and with which arguments. Shared by the CLI, which runs them attached to the terminal, and
    * `bleep.sourcegen` over MCP, which records their output instead: the MCP server's own stdin and stdout are its protocol channel.
    */
  def plan(build: model.Build, projectNames: Array[model.CrossProjectName]): List[Invocation] =
    projectNames.toList
      .flatMap(projectName => build.explodedProjects(projectName).sourcegen.values.toList.map { case script: model.ScriptDef.Main => (script, projectName) })
      .groupMap { case (s, _) => s } { case (_, pn) => pn }
      .toList
      .sortBy { case (script, _) => (script.project.value, script.main) }
      .map { case (script, forProjects) => Invocation(script, forProjects) }
}
