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
