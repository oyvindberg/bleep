package bleep.commands

import bleep.BleepExecutable

import java.nio.file.{Files, Path}

/** Coursier uses a wrapper script to call the bleep binary. This detects if it's a coursier installation where the wrapper script is present.
  */
object CoursierInstallation {
  def unapply(exec: BleepExecutable): Option[(Path, BleepExecutable.Binary)] =
    exec match {
      case binary: BleepExecutable.Binary =>
        val scriptPath = binary.command.getParent.resolve("bleep")
        val isCoursierPath = binary.command.toAbsolutePath.toString.contains("coursier")
        if (isCoursierPath && Files.exists(scriptPath)) {
          Some((scriptPath, binary))
        } else None
      case BleepExecutable.CurrentJava(_, _)    => None
      case BleepExecutable.DownloadedJava(_, _) => None
    }
}
