package bleep

abstract class BleepScriptRunner(override val scriptName: String) extends BleepScript {
  def main(args: Array[String]): Unit =
    bootstrap.forScript(scriptName)((started, commands) => runScript(started, commands, args.toList))
}
