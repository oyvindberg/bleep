package bleep
package internal

import io.github.alexarchambault.nativeterm.NativeTerminal
import ryddig.Logger

object TerminalInfo {
  private var terminalWidth: Option[Int] = None
  private var initialized: Boolean = false

  def initialize(logger: Logger): Unit = {
    if (initialized) return
    initialized = true

    try {
      // Enable ANSI support on Windows
      NativeTerminal.setupAnsi()

      // Get terminal size using native-terminal
      val size = NativeTerminal.getSize()
      if (size != null && size.getWidth() > 0) {
        terminalWidth = Some(size.getWidth())
        logger.debug(s"Terminal width detected: ${size.getWidth()}")
      } else {
        logger.debug(s"Terminal size not available or invalid: $size")
        // Try to get a sensible default from environment
        sys.env.get("COLUMNS").flatMap(_.toIntOption).filter(_ > 0) match {
          case Some(cols) =>
            terminalWidth = Some(cols)
            logger.debug(s"Using COLUMNS env var: $cols")
          case None =>
            logger.debug("No COLUMNS env var found")
        }
      }
    } catch {
      case e: Exception =>
        logger.debug(s"Failed to initialize native-terminal: ${e.getMessage}")
      // Continue without terminal info
    }
  }

  def getWidth: Option[Int] = terminalWidth

  def getWidthOrDefault(default: Int = 80): Int = terminalWidth.getOrElse(default)
}
