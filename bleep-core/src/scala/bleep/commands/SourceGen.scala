package bleep
package commands

import bleep.internal.TransitiveProjects

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

  /** Through the compile server's task graph, the same as the sourcegen a compile runs: each script after its script project and its `inputs` are built. Unlike
    * a compile, the named projects' generators run even when up to date. Running the scripts here with `Run` instead built only the script project, so a script
    * declaring `inputs` ran before the projects it reads existed — and `Script.run` refused it outright, its `inputs` check being meant for `bleep run`.
    */
  private def runOnce(started: Started): Either[BleepException, Unit] =
    ReactiveBsp.sourcegen(projectNames, DisplayMode.NoTui).run(started)
}

object SourceGen {

  /** One sourcegen script to run, and the projects it generates for. A script shared by several projects runs once, told about all of them. */
  case class Invocation(script: model.ScriptDef.Main, forProjects: List[model.CrossProjectName]) {
    def args: List[String] = forProjects.flatMap(pn => List("--project", pn.value))
  }

  /** Which scripts `bleep.sourcegen` over MCP runs for `projectNames`, and with which arguments. The MCP tool forks each one itself so it can record its
    * output: the MCP server's own stdin and stdout are its protocol channel. The CLI does not use this; it runs sourcegen through the compile server.
    */
  def plan(build: model.Build, projectNames: Array[model.CrossProjectName]): List[Invocation] =
    projectNames.toList
      .flatMap(projectName => build.explodedProjects(projectName).sourcegen.values.toList.map { case script: model.ScriptDef.Main => (script, projectName) })
      .groupMap { case (s, _) => s } { case (_, pn) => pn }
      .toList
      .sortBy { case (script, _) => (script.project.value, script.main) }
      .map { case (script, forProjects) => Invocation(script, forProjects) }
}
