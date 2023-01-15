package bleep
package commands

import bleep.bsp.BspProjectSelection
import bleep.internal.{jvmRunCommand, Argv0, FileUtils}
import ch.epfl.scala.bsp4j
import coursier.core.{ModuleName, Organization}
import io.circe.Encoder
import io.circe.syntax._

import java.nio.file.{Files, Path}
import java.util
import scala.jdk.CollectionConverters._

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
    val maybeExecutablePath = Option((new Argv0).get(null))
      .map {
        case absolute if absolute.startsWith("/") =>
          Path.of(absolute)
        case relative =>
          FileUtils.cwd / relative
      }
      .filter(Files.isRegularFile(_))
      .filter(Files.isExecutable)

    val cmd: List[String] =
      maybeExecutablePath match {
        case Some(executablePath) if !forceJvm => List(executablePath.toString)
        case _ =>
          val latestRelease = model.BleepVersion.current.latestRelease
          OsArch.current match {
            case hasNativeImage: OsArch.HasNativeImage if !forceJvm =>
              started.logger.warn(s"couldn't determine path of Bleep executable. Setting up version ${latestRelease.value} downloaded through coursier")
              val bleepExecutablePath = FetchBleepRelease(latestRelease, new BleepCacheLogger(started.logger), started.executionContext, hasNativeImage).orThrow
              List(bleepExecutablePath.toString)
            case other =>
              if (forceJvm) started.logger.info(s"Setting up BSP through a JVM as requested")
              else started.logger.warn(s"There is no graalvm native-image for $other. Setting up BSP through a JVM")
              val bleepCli = model.Dep.ScalaDependency(Organization("build.bleep"), ModuleName("bleep-cli"), latestRelease.value, fullCrossVersion = false)
              val resolved = started.resolver
                .force(
                  Set(bleepCli),
                  model.VersionCombo.Jvm(model.VersionScala.Scala213),
                  s"resolving bleep ${latestRelease.value} from maven central"
                )
              jvmRunCommand.cmd(started.jvmCommand, Nil, resolved.jars, "bleep.Main", Nil)
          }
      }

    val details = new bsp4j.BspConnectionDetails(
      "bleep",
      (cmd ++ List("bsp")).asJava,
      model.BleepVersion.current.value,
      scala.build.blooprifle.internal.Constants.bspVersion,
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
