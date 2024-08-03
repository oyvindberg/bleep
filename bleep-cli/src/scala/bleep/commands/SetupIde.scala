package bleep
package commands

import bleep.bsp.BspProjectSelection
import bleep.internal.FileUtils
import ch.epfl.scala.bsp4j
import io.circe.Encoder
import io.circe.syntax.*

import java.nio.file.Files
import java.util
import scala.jdk.CollectionConverters.*

case class SetupIde(maybeSelectedProjects: Option[List[String]], forceJvm: Boolean) extends BleepBuildCommand {
  implicit def encodesUtilList[T: Encoder]: Encoder[util.List[T]] = Encoder[List[T]].contramap(_.asScala.toList)
  implicit val encoder: Encoder[bsp4j.BspConnectionDetails] =
    Encoder.forProduct5[bsp4j.BspConnectionDetails, String, util.List[String], String, String, util.List[String]](
      "name",
      "argv",
      "version",
      "bspVersion",
      "languages"
    )(x => (x.getName, x.getArgv, x.getVersion, x.getBspVersion, x.getLanguages))

  override def run(started: Started): Either[BleepException, Unit] = {
    val details = new bsp4j.BspConnectionDetails(
      "bleep",
      (started.bleepExecutable.forceGet.whole ++ List("bsp")).asJava,
      model.BleepVersion.current.value,
      bloop.rifle.internal.BuildInfo.bspVersion,
      List("scala", "java").asJava
    )

    BspProjectSelection.store(started.logger, started.buildPaths, maybeSelectedProjects)

    List(
      // remove other configured BSP tools
      Option(started.buildPaths.buildDir / ".bsp").filter(FileUtils.exists).filter { p =>
        if (Files.isDirectory(p)) {
          Files.list(p).toList.asScala.toList match {
            case one :: Nil if one.getFileName.toString == "bleep.json" => false
            case _                                                      => true
          }
        } else true
      },
      // causes intellij to always pick sbt BSP import
      Some(started.buildPaths.buildDir / ".bloop").filter(FileUtils.exists),
      // cause metals to always pick sbt BSP import
      Some(started.buildPaths.buildDir / "build.sbt").filter(FileUtils.exists),
      Some(started.buildPaths.buildDir / "project").filter(FileUtils.exists)
    ).flatten match {
      case Nil => ()
      case conflicts =>
        LazyList
          .from(0)
          .map(n => started.buildPaths.buildDir / s"bleep-moved-files-$n")
          .find(target => !FileUtils.exists(target))
          .foreach { target =>
            Files.createDirectories(target)
            conflicts.foreach(conflict => Files.move(conflict, target / conflict.getFileName.toString))
            started.logger.info(s"Moved ${conflicts.mkString(", ")} into $target to avoid IDE picking wrong build tool when importing")
          }
    }

    Right(FileUtils.writeString(started.logger, Some("writing BSP connection file"), started.buildPaths.bspBleepJsonFile, details.asJson.spaces2))
  }
}
