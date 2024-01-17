package bleep
package commands


object Version extends BleepNoBuildCommand {
  override def run(): Either[BleepException, Unit] = {
    println(s"bleep ${model.BleepVersion.current.value}")
    Right(())
  }
}
