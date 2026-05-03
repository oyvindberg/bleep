package bleep
package scripts

import ryddig.Logger
import bleep.plugin.dynver.DynVerPlugin

import java.nio.file.Files

object GenerateResources extends BleepCodegenScript("GenerateResources") {
  override def run(started: Started, commands: Commands, targets: List[GenerateResources.Target], args: List[String]): Unit = {
    val dynVer = new DynVerPlugin(baseDirectory = started.buildPaths.buildDir.toFile, dynverSonatypeSnapshots = true)
    val buildJvm = started.build.jvm.getOrElse(sys.error("Bleep should have a defined JVM in build file"))

    targets.foreach { target =>
      target.project.name.value match {
        case "bleep-model" =>
          writeVersion(target, started.logger, dynVer.version)
          writeJvm(target, started.logger, buildJvm)
        case "bleepscript" =>
          writeBleepscriptVersion(target, started.logger, dynVer.version)
        case other =>
          sys.error(s"GenerateResources doesn't know what to do for project '$other'")
      }
    }
  }

  // writeIfChanged: compare content before writing to avoid unnecessary timestamp changes.
  // This is redundant once BleepCodegenScript's framework-level write-if-changed is released,
  // but needed for local dev where scripts-init uses the released bleep-core ($version in bleep.yaml).
  private def writeIfChanged(to: java.nio.file.Path, content: String, logger: Logger, project: String): Unit = {
    Files.createDirectories(to.getParent)
    val existing = if (Files.exists(to)) Files.readString(to) else ""
    if (existing == content) {
      logger.withContext("project", project).info(s"Up to date: $to")
    } else {
      logger.withContext("project", project).warn(s"Writing $to")
      Files.writeString(to, content): Unit
    }
  }

  def writeVersion(target: Target, logger: Logger, version: String): Unit = {
    val to = target.sources / "bleep/model/BleepVersion.scala"
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
    writeIfChanged(to, content, logger, target.project.value)
  }
  def writeBleepscriptVersion(target: Target, logger: Logger, version: String): Unit = {
    val to = target.sources / "bleepscript/BleepscriptVersion.java"
    val content =
      s"""|//
          |// GENERATED FILE!
          |//
          |package bleepscript;
          |
          |/**
          | * The bleep version this bleepscript jar was published with. Used by
          | * {@link BleepscriptServices.Holder#bootstrap} to fetch the matching bleep-core_3
          | * artifact when no implementation is found on the classpath.
          | */
          |public final class BleepscriptVersion {
          |  private BleepscriptVersion() {}
          |  public static final String VALUE = "$version";
          |}
          |""".stripMargin
    writeIfChanged(to, content, logger, target.project.value)
  }

  def writeJvm(target: Target, logger: Logger, buildJvm: model.Jvm): Unit = {
    val to = target.sources / "bleep/model/Jvm.scala"
    val content =
      s"""|//
          |// GENERATED FILE!
          |//
          |package bleep.model
          |
          |import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
          |import io.circe.{Decoder, Encoder}
          |
          |case class Jvm(name: String, index: Option[String]) {
          |  /** Major Java version parsed from `name` (e.g. "graalvm-community:25.0.1" → "25"). */
          |  def majorVersion: String = {
          |    val tag = name.dropWhile(_ != ':').drop(1)
          |    if (tag.isEmpty) sys.error(s"Could not extract major version from JVM '$$name'")
          |    tag.takeWhile(c => c != '.' && c != '-')
          |  }
          |}
          |
          |object Jvm {
          |  val graalvm = Jvm("${buildJvm.name}", None)
          |  val system = Jvm("system", None)
          |  implicit val encodes: Encoder[Jvm] = deriveEncoder
          |  implicit val decodes: Decoder[Jvm] = deriveDecoder
          |
          |  def isSystem(jvm: Jvm): Boolean = jvm == system
          |}
          |""".stripMargin
    writeIfChanged(to, content, logger, target.project.value)
  }
}
