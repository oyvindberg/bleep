package bleep

import bleep.rewrites.BuildRewrite

import java.nio.file.Path

abstract class BleepCodegenScript(val scriptName: String) {
  // can override this in subclass
  val rewrites: List[BuildRewrite] = Nil

  val ThisClassName = getClass.getName.split('$').head

  case class Target(project: model.CrossProjectName, sources: Path, resources: Path)

  def main(args: Array[String]): Unit = {
    val (commonOpts, restArgs) = CommonOpts.parse(args.toList)
    val (codegenOpts, restArgs2) = CodegenOpts.parse(restArgs)

    bootstrap.forScript(scriptName, commonOpts, rewrites) { (started, commands) =>
      val targets = codegenOpts.projectNames.map { projectName =>
        Target(
          projectName,
          sources = started.buildPaths.generatedSourcesDir(projectName, ThisClassName),
          resources = started.buildPaths.generatedResourcesDir(projectName, ThisClassName)
        )
      }
      run(started, commands, targets, restArgs2)
    }
  }

  def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit
}
