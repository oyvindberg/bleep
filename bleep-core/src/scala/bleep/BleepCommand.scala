package bleep

trait BleepNoBuildCommand {
  def run(): Either[BleepException, Unit]
}

trait BleepBuildCommand {
  def run(started: Started): Either[BleepException, Unit]
}

trait BleepCommand extends BleepNoBuildCommand with BleepBuildCommand {
  final def run(started: Started): Either[BleepException, Unit] = run()
}
