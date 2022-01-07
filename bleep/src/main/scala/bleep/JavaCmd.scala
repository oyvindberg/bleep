package bleep

import java.nio.file.{Files, Paths}

object JavaCmd {
  val defaultCommand = if (sys.props("os.name").toLowerCase.contains("win")) "java" else "/usr/bin/java"
  val detected =
    sys.env.get("JAVA_HOME").map(Paths.get(_)).map(_.resolve("bin").resolve("java")).filter(Files.exists(_)).map(_.toString)
  val javacommand = detected.getOrElse(defaultCommand)
}
