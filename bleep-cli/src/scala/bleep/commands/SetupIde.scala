package bleep
package commands

import bleep.bsp.ProjectSelection
import bleep.internal.{Argv0, FileUtils}
import bleep.logging.Logger
import ch.epfl.scala.bsp4j
import io.circe.Encoder
import io.circe.syntax._

import java.nio.file.Files
import java.util
import scala.jdk.CollectionConverters._

case class SetupIde(buildPaths: BuildPaths, logger: Logger, maybeSelectedProjects: Option[List[String]]) extends BleepCommand {
  implicit def encodesUtilList[T: Encoder]: Encoder[util.List[T]] = Encoder[List[T]].contramap(_.asScala.toList)
  implicit val encoder: Encoder[bsp4j.BspConnectionDetails] =
    Encoder.forProduct5[bsp4j.BspConnectionDetails, String, util.List[String], String, String, util.List[String]](
      "name",
      "argv",
      "version",
      "bspVersion",
      "languages"
    )(x => (x.getName, x.getArgv, x.getVersion, x.getBspVersion, x.getLanguages))

  override def run(): Either[BuildException, Unit] = {
    val progName = (new Argv0).get("bleep")
    val details = new bsp4j.BspConnectionDetails(
      "bleep",
      List(progName, "bsp").asJava,
      BleepVersion.version,
      scala.build.blooprifle.internal.Constants.bspVersion,
      List("scala", "java").asJava
    )

    ProjectSelection.store(buildPaths, maybeSelectedProjects)

    List(
      // remove other configured BSP tools
      Option(buildPaths.buildDir / ".bsp").filter { p =>
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

    FileUtils.writeString(buildPaths.bspBleepJsonFile, details.asJson.spaces2)
    Right(logger.info(s"Wrote file ${buildPaths.bspBleepJsonFile}"))
  }
}
