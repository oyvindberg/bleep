package bleep

import bleep.rewrites.BuildRewrite
import bleep.model.CrossProjectName

abstract class BleepCodegenScript(val scriptName: String) {
  // can override this in subclass
  val rewrites: List[BuildRewrite] = Nil

  def main(args: Array[String]): Unit = {
    val (commonOpts, restArgs) = CommonOpts.parse(args.toList)
    val (codegenOpts, restArgs2) = CodegenOpts.parse(restArgs)
    import codegenOpts.crossName
    bootstrap.forScript(scriptName, commonOpts, rewrites) { (started, commands) =>
      run(started, commands, crossName, restArgs2)
    }
  }

  def run(started: Started, commands: Commands, crossName: CrossProjectName, args: List[String]): Unit

}
