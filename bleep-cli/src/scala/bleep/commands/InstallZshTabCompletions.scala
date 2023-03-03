package bleep
package commands

import bleep.internal.FileUtils
import bleep.logging.Logger

import java.nio.charset.StandardCharsets
import java.nio.file.Path

case class InstallZshTabCompletions(userPaths: UserPaths, logger: Logger) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    val programName = BleepExecutable.findCurrentBleep(logger) match {
      case Some(binary: BleepExecutable.Binary) => binary.command.getFileName.toString
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

    val completionScriptDest = userPaths.configDir / "zsh" / s"_$programName"

    logger.info(s"Writing $completionScriptDest")
    FileSync.softWriteBytes(completionScriptDest, completionScript.getBytes(StandardCharsets.UTF_8))

    val zshRc = Option(System.getenv("ZDOTDIR")).map(Path.of(_)).getOrElse(FileUtils.Home) / ".zshrc"

    PatchRcFile(None, logger, zshRc)(
      s"""fpath=("$completionScriptDest" $$fpath)
         |compinit
         |""".stripMargin
    )

    Right(())
  }
}
