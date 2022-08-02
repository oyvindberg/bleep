package bleep

trait BleepCommand {
  def run(): Either[BleepException, Unit]
}
