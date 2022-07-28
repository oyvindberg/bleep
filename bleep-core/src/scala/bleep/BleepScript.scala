package bleep

trait BleepScript {
  def scriptName: String
  def runScript(started: Started, commands: Commands, args: List[String]): Unit
}
