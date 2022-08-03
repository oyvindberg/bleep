package bleep

abstract class BleepScriptRunner(val scriptName: String) extends BleepScript {
  def main(args: Array[String]): Unit =
    bootstrap.forScript(scriptName)((started, commands) => run(started, commands, args.toList))

  def run(started: Started, commands: Commands, args: List[String]): Unit
}
