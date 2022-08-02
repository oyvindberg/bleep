package bleep.scripts

import bleep.Started
import bleep.tasks.publishing.DynVerPlugin

import java.nio.file.Files

object GenerateResources extends App {
  bleep.bootstrap.forScript("GenerateResources") { (started, _) =>
    val dynVer = new DynVerPlugin(baseDirectory = started.buildPaths.buildDir.toFile, dynverSonatypeSnapshots = true)
    writeGenerated(started, dynVer.version)
  }

  def writeGenerated(started: Started, version: String): Unit =
    started.build.projects.foreach {
      case (crossName, _) if crossName.name.value == "bleep-model" =>
        val to = started.buildPaths.generatedSourcesDir(crossName).resolve("bleep/model/BleepVersion.scala")
        started.logger.withContext(crossName).warn(s"Writing $to")
        val content =
          s"""|//
              |// GENERATED FILE!
              |//
              |package bleep.model
              |
              |import io.circe.{Decoder, Encoder}
              |
              |case class BleepVersion(value: String) extends AnyVal
              |
              |object BleepVersion {
              |  val dev = BleepVersion("dev")
              |  val current = BleepVersion("$version")
              |  implicit val ordering: Ordering[BleepVersion] = Ordering.by(_.value)
              |  implicit val encodes: Encoder[BleepVersion] = Encoder[String].contramap(_.value)
              |  implicit val decodes: Decoder[BleepVersion] = Decoder[String].map(BleepVersion.apply)
              |}""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
        ()
      case _ => ()
    }
}
