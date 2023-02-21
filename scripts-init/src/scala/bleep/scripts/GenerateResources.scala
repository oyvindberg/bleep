package bleep
package scripts

import bleep.plugin.dynver.DynVerPlugin

import java.nio.file.Files

// todo: revert to using `BleepCodegenScript` after 0.0.1-M26.
// the interface is changing to accommodate more than one project at a time, plus the source generation folders change
// and in order to build bleep it needs to be compatible with both M25 and M26.
// this will be easier in downstream projects which will just bump bleep version, fix compile errors and go on
object GenerateResources {
  def main(args: Array[String]): Unit = {
    val (commonOpts, restArgs) = CommonOpts.parse(args.toList)
    val (codegenOpts, _) = GenerateResources.CodegenOpts.parse(restArgs)

    bootstrap.forScript("GenerateResources", commonOpts, Nil) { (started, _) =>
      val dynVer = new DynVerPlugin(baseDirectory = started.buildPaths.buildDir.toFile, dynverSonatypeSnapshots = true)
      codegenOpts.projectNames.foreach { crossName =>
        writeGenerated(started, crossName, dynVer.version)
      }
    }
  }

  def writeGenerated(started: Started, crossName: model.CrossProjectName, version: String): Unit = {
    val tos = List(
      started.buildPaths.dotBleepDir / "generated-sources" / crossName.value / "bleep.scripts.GenerateResources/bleep/model/BleepVersion.scala"
    )
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

    tos.foreach { to =>
      started.logger.withContext(crossName).warn(s"Writing $to")
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }
  }

  // copied from bleep-core
  case class CodegenOpts(
      projectNames: List[model.CrossProjectName]
  )

  object CodegenOpts {
    // we apparently need to parse (and remove from input) before the rest of the app is launched
    def parse(args: List[String]): (CodegenOpts, List[String]) = {
      val projects = List.newBuilder[String]
      val keepArgs = List.newBuilder[String]
      var idx = 0
      while (idx < args.length) {
        args(idx) match {
          case "-p" | "--project" if args.isDefinedAt(idx + 1) =>
            projects += args(idx + 1)
            idx += 1
          case "--" =>
            keepArgs ++= args.drop(idx)
            idx = Int.MaxValue - 1
          case other => keepArgs += other
        }
        idx += 1
      }

      projects.result() match {
        case Nil => throw new IllegalAccessException("A project name should have been passed")
        case nonEmptyProjectStrings =>
          val projectNames = nonEmptyProjectStrings.map { p =>
            model.CrossProjectName.fromString(p).getOrElse {
              throw new IllegalArgumentException(s"Illegal crossName : $p")
            }
          }
          (CodegenOpts(projectNames), keepArgs.result())
      }
    }
  }
}
