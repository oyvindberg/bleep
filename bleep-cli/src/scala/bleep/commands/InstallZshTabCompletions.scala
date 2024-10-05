package bleep
package commands

import bleep.internal.FileUtils
import ryddig.Logger

import java.nio.charset.StandardCharsets
import java.nio.file.Path

case class InstallZshTabCompletions(userPaths: UserPaths, logger: Logger, stdout: Boolean) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    val programName = BleepExecutable.findCurrentBleep(logger) match {
      case Some(CoursierInstallation(scriptPath, _)) => scriptPath.getFileName.toString
      case Some(binary: BleepExecutable.Binary)      => binary.command.getFileName.toString
      case _ =>
        logger.warn("Doesn't know name of a Bleep executable. Falling back to 'bleep'")
        "bleep"
    }

    val completionScript =
      s"""#compdef _$programName $programName
         |
         |function _$programName {
         |  eval "$$($programName _complete-zsh $$CURRENT $$words[@])"
         |}
         |""".stripMargin

    val completionScriptDir = userPaths.configDir / "zsh"
    val completionScriptDest = completionScriptDir / s"_$programName"

    if (stdout) {
      println(completionScript)
    } else {
      logger.info(s"Writing $completionScriptDest")
      FileSync.softWriteBytes(completionScriptDest, completionScript.getBytes(StandardCharsets.UTF_8)).discard()
      val zshRc = Option(System.getenv("ZDOTDIR")).map(Path.of(_)).getOrElse(FileUtils.Home) / ".zshrc"

      PatchRcFile(None, logger, zshRc)(
        s"""fpath=("$completionScriptDir" $$fpath)
         |compinit
         |""".stripMargin
      )
    }

    Right(())
  }
}
