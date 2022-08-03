package bleep

trait BleepScript {
  def scriptName: String
  def run(started: Started, commands: Commands, args: List[String]): Unit
}
