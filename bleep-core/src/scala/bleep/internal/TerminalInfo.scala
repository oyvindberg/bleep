package bleep
package internal

import ryddig.Logger
import scala.util.Try

object TerminalInfo {
  private var terminalWidth: Option[Int] = None

  def initialize(logger: Logger): Unit =
    try {
      // Try to get terminal width from system
      terminalWidth = getTerminalWidth()
      logger.debug(s"Terminal width detected: ${terminalWidth.getOrElse("unknown")}")
    } catch {
      case e: Exception =>
        logger.debug(s"Failed to detect terminal width: ${e.getMessage}")
      // Continue without terminal info
    }

  private def getTerminalWidth(): Option[Int] =
    // Try different methods to get terminal width
    getWidthFromEnv()
      .orElse(getWidthFromTput())
      .orElse(getWidthFromStty())

  private def getWidthFromEnv(): Option[Int] =
    sys.env.get("COLUMNS").flatMap(s => Try(s.toInt).toOption)

  private def getWidthFromTput(): Option[Int] =
    try {
      import sys.process._
      val result = "tput cols".!!.trim
      Try(result.toInt).toOption
    } catch {
      case _: Exception => None
    }

  private def getWidthFromStty(): Option[Int] =
    try {
      import sys.process._
      val result = "stty size".!!.trim
      val parts = result.split(" ")
      if (parts.length >= 2) Try(parts(1).toInt).toOption else None
    } catch {
      case _: Exception => None
    }

  def getWidth: Option[Int] = terminalWidth

  def getWidthOrDefault(default: Int = 80): Int = terminalWidth.getOrElse(default)
}
