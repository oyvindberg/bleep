package bleep
package scripts

import ryddig.Logger

import java.nio.file.Files

object GenerateResources extends BleepCodegenScript("GenerateResources") {
  override def run(started: Started, commands: Commands, targets: List[GenerateResources.Target], args: List[String]): Unit = {
    val buildJvm = started.build.jvm.getOrElse(sys.error("Bleep should have a defined JVM in build file"))

    targets.foreach { target =>
      target.project.name.value match {
        case "bleep-model" =>
          // Versions are not generated here any more: `BleepVersion` and `BleepscriptVersion` read their project's `dynver` stamp. A version generated into a
          // source went stale silently and put a per-commit value into the cache key of the project and everything downstream of it.
          writeJvm(target, started.logger, buildJvm)
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
