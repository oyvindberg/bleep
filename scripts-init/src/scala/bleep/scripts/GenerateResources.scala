package bleep
package scripts

import bleep.logging.Logger
import bleep.plugin.dynver.DynVerPlugin

import java.nio.file.Files

object GenerateResources extends BleepCodegenScript("GenerateResources") {
  override def run(started: Started, commands: Commands, targets: List[GenerateResources.Target], args: List[String]): Unit = {
    val dynVer = new DynVerPlugin(baseDirectory = started.buildPaths.buildDir.toFile, dynverSonatypeSnapshots = true)
    targets.foreach { target =>
      writeGenerated(target, started.logger, dynVer.version)
    }
  }

  def writeGenerated(target: Target, logger: Logger, version: String): Unit = {
    val to = target.sources / "bleep/model/BleepVersion.scala"
    logger.withContext(target.project).warn(s"Writing $to")
    val content =
      s"""|//
          |// GENERATED FILE!
          |//
          |package bleep.model
          |
          |import io.circe.{Decoder, Encoder}
          |
          |case class BleepVersion(value: String) extends AnyVal {
          |  def latestRelease: BleepVersion = BleepVersion(value.split("\\\\+").head)
          |  def isDevelopment: Boolean = latestRelease.value != value
          |}
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
  }
}
