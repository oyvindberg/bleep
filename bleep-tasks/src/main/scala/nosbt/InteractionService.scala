package nosbt

abstract class InteractionService {

  /** Prompts the user for input, optionally with a mask for characters. */
  def readLine(prompt: String, mask: Boolean): Option[String]

  /** Ask the user to confirm something (yes or no) before continuing. */
  def confirm(msg: String): Boolean

  def terminalWidth: Int

  def terminalHeight: Int

  // TODO - Ask for input with autocomplete?
}

object InteractionService {
  // todo: mask password when typing
  object DoesNotMaskYourPasswordExclamationOneOne extends InteractionService {

    /** Prompts the user for input, optionally with a mask for characters. */
    override def readLine(prompt: String, mask: Boolean): Option[String] = {
      Console.print(prompt)
      Some(Console.in.readLine())
    }

    /** Ask the user to confirm something (yes or no) before continuing. */
    override def confirm(msg: String): Boolean = {
      Console.print(msg)
      Console.in.readLine() == "yes"
    }

    override def terminalWidth: Int = -1

    override def terminalHeight: Int = -1
  }
}
