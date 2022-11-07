package bleep

abstract class BleepScript(val scriptName: String) {
  def main(args: Array[String]): Unit = {
    val (commonOpts, restArgs) = CommonOpts.parse(args.toList)
    bootstrap.forScript(scriptName, commonOpts)((started, commands) => run(started, commands, restArgs))
  }

  def run(started: Started, commands: Commands, args: List[String]): Unit
}
