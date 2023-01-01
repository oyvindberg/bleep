package bleep
package commands

import bleep.bsp.BspProjectSelection
import bleep.internal.{fatal, Argv0, FileUtils}
import bleep.logging.Logger
import ch.epfl.scala.bsp4j
import io.circe.Encoder
import io.circe.syntax._

import java.nio.file.{Files, Path}
import java.util
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters._

case class SetupIde(buildPaths: BuildPaths, logger: Logger, maybeSelectedProjects: Option[List[String]], ec: ExecutionContext) extends BleepCommand {
  implicit def encodesUtilList[T: Encoder]: Encoder[util.List[T]] = Encoder[List[T]].contramap(_.asScala.toList)
  implicit val encoder: Encoder[bsp4j.BspConnectionDetails] =
    Encoder.forProduct5[bsp4j.BspConnectionDetails, String, util.List[String], String, String, util.List[String]](
      "name",
      "argv",
      "version",
      "bspVersion",
      "languages"
    )(x => (x.getName, x.getArgv, x.getVersion, x.getBspVersion, x.getLanguages))

  override def run(): Either[BleepException, Unit] = {
    val maybeExecutablePath = Option((new Argv0).get(null))
      .map {
        case absolute if absolute.startsWith("/") =>
          Path.of(absolute)
        case relative =>
          FileUtils.cwd / relative
      }
      .filter(Files.isRegularFile(_))
      .filter(Files.isExecutable)

    val bleepExecutablePath: Path =
      maybeExecutablePath.getOrElse {
        val latestRelease = model.BleepVersion.current.latestRelease
        logger.warn(s"couldn't determine name of Bleep executable. Setting up version ${latestRelease.value}")
        OsArch.current match {
          case hasNativeImage: OsArch.HasNativeImage =>
            FetchBleepRelease(latestRelease, new BleepCacheLogger(logger), ec, hasNativeImage).orThrow
          case other =>
            fatal(
              s"No native image available for $other, so the bleep is not able to perform setup-ide. See https://github.com/oyvindberg/bleep/issues/260 for how you can help out",
              logger
            )
        }

      }

    val details = new bsp4j.BspConnectionDetails(
      "bleep",
      List(bleepExecutablePath.toString, "bsp").asJava,
      model.BleepVersion.current.value,
      scala.build.blooprifle.internal.Constants.bspVersion,
      List("scala", "java").asJava
    )

    BspProjectSelection.store(logger, buildPaths, maybeSelectedProjects)

    List(
      // remove other configured BSP tools
      Option(buildPaths.buildDir / ".bsp").filter(FileUtils.exists).filter { p =>
        if (Files.isDirectory(p)) {
          Files.list(p).toList.asScala.toList match {
            case one :: Nil if one.getFileName.toString == "bleep.json" => false
            case _                                                      => true
          }
        } else true
      },
      // causes intellij to always pick sbt BSP import
      Some(buildPaths.buildDir / ".bloop").filter(FileUtils.exists),
      // cause metals to always pick sbt BSP import
      Some(buildPaths.buildDir / "build.sbt").filter(FileUtils.exists),
      Some(buildPaths.buildDir / "project").filter(FileUtils.exists)
    ).flatten match {
      case Nil => ()
      case conflicts =>
        LazyList
          .from(0)
          .map(n => buildPaths.buildDir / s"bleep-moved-files-$n")
          .find(target => !FileUtils.exists(target))
          .foreach { target =>
            Files.createDirectories(target)
            conflicts.foreach(conflict => Files.move(conflict, target / conflict.getFileName.toString))
            logger.info(s"Moved ${conflicts.mkString(", ")} into $target to avoid IDE picking wrong build tool when importing")
          }
    }

    Right(FileUtils.writeString(logger, Some("writing BSP connection file"), buildPaths.bspBleepJsonFile, details.asJson.spaces2))
  }
}
