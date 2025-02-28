package bleep
package commands

import bleep.internal.FileUtils
import ryddig.Logger

import java.nio.file.{Files, Path}

object PatchRcFile {
  def apply(customProgramName: Option[String], logger: Logger, rcFile: Path)(script: String): Unit = {
    val sep = customProgramName match {
      case None              => "\n# Bleep managed\n"
      case Some(programName) => s"\n# Bleep managed ($programName)\n"
    }
    val section = sep + script + sep
    val maybeNewContents = if (FileUtils.exists(rcFile)) {
      val oldContents = Files.readString(rcFile)
      oldContents.split(sep) match {
        case Array(before, _, after) =>
          Some(before + section + after).filterNot(_ == oldContents)
        case Array(before, _) =>
          Some(before + section)
        case Array(one) =>
          Some(one + section)
        case _ =>
          throw new BleepException.Text(s"didn't understand how to patch $rcFile. Try to remove everything bleep-related in it.")
      }

    } else Some(section)

    maybeNewContents match {
      case Some(writeContents) =>
        FileUtils.writeString(logger, Some("Adding tab completions"), rcFile, writeContents)
      case None =>
        logger.info(s"Tab completions already installed in $rcFile")
    }
  }
}
