package bleep

trait BleepCommand {
  def run(): Either[BuildException, Unit]
}
