package bleep
package commands

import bleep.internal.FileUtils
import bleep.logging.Logger

import java.nio.file.{Files, Path}

case class InstallTabCompletions(logger: Logger) extends BleepCommand {
  def reloadNotice() = logger.warn(s"Start a new shell or run `source ${InstallTabCompletions.dotProfile}` to enable")

  override def run(): Either[BleepException, Unit] = {
    if (FileUtils.exists(InstallTabCompletions.dotProfile)) {
      val oldContents = Files.readString(InstallTabCompletions.dotProfile)

      InstallTabCompletions.patch(oldContents) match {
        case Some(patched) if patched == oldContents =>
          logger.warn(s"Not touching ${InstallTabCompletions.dotProfile} since it already had tab completions")
        case Some(patched) =>
          logger.warn(s"Adding tab completions to ${InstallTabCompletions.dotProfile}")
          Files.writeString(InstallTabCompletions.dotProfile, patched)
          reloadNotice()
        case None =>
          throw new BleepException.Text(
            s"Couldn't update Bleep managed part of ${InstallTabCompletions.dotProfile}. You can add the script yourself: \n ${InstallTabCompletions.Script}"
          )
      }
    } else {
      logger.warn(s"Writing new ${InstallTabCompletions.dotProfile}")
      Files.writeString(InstallTabCompletions.dotProfile, InstallTabCompletions.Section)
      reloadNotice()
    }
    Right(())
  }
}

object InstallTabCompletions {
  val dotProfile = Path.of(sys.props("user.home")) / ".profile"

  val Script =
    """_bleep_completions() {
      |  COMPREPLY=($(bleep _complete "${COMP_LINE}" "${COMP_CWORD}" "${COMP_POINT}"))
      |}
      |
      |complete -F _bleep_completions bleep""".stripMargin

  val Sep = "\n# Bleep managed\n"
  val Section = Sep + Script + Sep

  def patch(contents: String): Option[String] =
    contents.split(Sep) match {
      case Array(before, _, after) => Some(before + Section + after)
      case Array(before, _)        => Some(before + Section)
      case Array(one)              => Some(one + Section)
      case other =>
        println(other.toList)
        None
    }
}
