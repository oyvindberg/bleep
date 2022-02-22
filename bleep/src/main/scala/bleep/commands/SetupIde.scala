package bleep
package commands

import bleep.bsp.ProjectSelection
import bleep.internal.Argv0
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

  override def run(): Unit = {
    val progName = (new Argv0).get("bleep")
    val details = new bsp4j.BspConnectionDetails(
      "bleep",
      List(progName, "bsp").asJava,
      constants.version,
      scala.build.blooprifle.internal.Constants.bspVersion,
      List("scala", "java").asJava
    )

    ProjectSelection.store(buildPaths, maybeSelectedProjects)

    Files.createDirectories(buildPaths.bspBleepJsonFile.getParent)
    Files.writeString(
      buildPaths.bspBleepJsonFile,
      details.asJson.spaces2
    )
    logger.info(s"Wrote file ${buildPaths.bspBleepJsonFile}")
  }
}
