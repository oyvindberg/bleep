package bleep

abstract class BleepScript(val scriptName: String) {
  def main(args: Array[String]): Unit =
    bootstrap.forScript(scriptName)((started, commands) => run(started, commands, args.toList))

  def run(started: Started, commands: Commands, args: List[String]): Unit
}
