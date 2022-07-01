package bleep.scripts

import bleep.Started
import bleep.tasks.publishing.DynVerPlugin

import java.nio.file.Files

object GenerateResources extends App {
  bleep.bootstrap.forScript("GenerateResources") { case (started, commands) =>
    val dynVer = new DynVerPlugin(baseDirectory = started.buildPaths.buildDir.toFile, dynverSonatypeSnapshots = true)
    writeGenerated(started, dynVer.version)
  }

  def writeGenerated(started: Started, version: String): Unit =
    started.build.projects.collect {
      case (crossName, _) if crossName.name.value == "bleep-core" =>
        val to = started.buildPaths.generatedSourcesDir(crossName).resolve("sbt-buildinfo/BleepVersion.scala")
        started.logger.withContext(crossName).warn(s"Writing $to")
        val content =
          s"""|package bleep
              |
              |/** This object was generated. */
              |case object BleepVersion {
              |  val version: String = "$version"
              |  override val toString: String = "version: $version"
              |}""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
        ()
    }
}
