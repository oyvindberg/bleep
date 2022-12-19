package bleep

import bleep.rewrites.BuildRewrite

abstract class BleepScript(val scriptName: String) {
  // can override this in subclass
  val rewrites: List[BuildRewrite] = Nil

  def main(args: Array[String]): Unit = {
    val (commonOpts, restArgs) = CommonOpts.parse(args.toList)
    bootstrap.forScript(scriptName, commonOpts, rewrites)((started, commands) => run(started, commands, restArgs))
  }

  def run(started: Started, commands: Commands, args: List[String]): Unit
}
