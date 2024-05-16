package bleep
package commands

import bleep.internal.FileUtils
import bleep.logging.Logger

case class InstallBashTabCompletions(logger: Logger) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    val programName = BleepExecutable.findCurrentBleep(logger) match {
      case Some(CoursierInstallation(scriptPath, _)) => scriptPath.getFileName.toString
      case Some(binary: BleepExecutable.Binary)      => binary.command.getFileName.toString
      case _ =>
        logger.warn("Doesn't know name of a Bleep executable. Falling back to 'bleep'")
        "bleep"
    }
    val customProgramName = if (programName == "bleep") None else Some(programName)

    PatchRcFile(customProgramName, logger, FileUtils.Home / ".profile")(
      s"""_${programName}_completions() {
        |  COMPREPLY=($$(bleep _complete "$${COMP_LINE}" "$${COMP_CWORD}" "$${COMP_POINT}"))
        |}
        |
        |complete -F _${programName}_completions $programName""".stripMargin
    )

    Right(())
  }
}
