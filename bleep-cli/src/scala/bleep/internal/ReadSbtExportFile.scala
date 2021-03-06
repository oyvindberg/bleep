package bleep.internal

import bleep.BuildException
import sbt.librarymanagement.{ModuleID, ScalaVersion}
import sjsonnew.support.scalajson.unsafe.{Converter, Parser}
import sjsonnew.{Builder, JsonFormat, Unbuilder}

import java.nio.file.Path
import scala.util.{Failure, Success}

object ReadSbtExportFile {
  def parse(path: Path, jsonStr: String): ExportedProject =
    Parser.parseFromString(jsonStr).flatMap(Converter.fromJson[ExportedProject](_)) match {
      case Failure(exception) => throw new BuildException.InvalidJson(path, exception)
      case Success(value)     => value
    }

  case class ExportedProject(organization: String, bloopName: String, sbtName: String, scalaVersion: ScalaVersion, dependencies: Seq[ModuleID])

  object ExportedProject {
    import sbt.librarymanagement.LibraryManagementCodec._

    implicit val format: JsonFormat[ExportedProject] = new JsonFormat[ExportedProject] {
      override def read[J](jsOpt: Option[J], unbuilder: Unbuilder[J]): ExportedProject =
        jsOpt match {
          case Some(j) =>
            unbuilder.beginObject(j)
            val organization = unbuilder.readField[String]("organization")
            val bloopName = unbuilder.readField[String]("bloopName")
            val sbtName = unbuilder.readField[String]("sbtName")
            val scalaFullVersion = unbuilder.readField[String]("scalaFullVersion")
            val scalaBinaryVersion = unbuilder.readField[String]("scalaBinaryVersion")
            val dependencies = unbuilder.readField[List[ModuleID]]("dependencies")
            unbuilder.endObject()

            ExportedProject(organization, bloopName, sbtName, scalaVersion = ScalaVersion(scalaFullVersion, scalaBinaryVersion), dependencies = dependencies)
          case None =>
            sjsonnew.deserializationError("expected a json value to read")
        }

      override def write[J](obj: ExportedProject, builder: Builder[J]): Unit = {
        builder.beginObject()
        builder.addField("organization", obj.organization)
        builder.addField("bloopName", obj.bloopName)
        builder.addField("sbtName", obj.sbtName)
        builder.addField("scalaFullVersion", obj.scalaVersion.full)
        builder.addField("scalaBinaryVersion", obj.scalaVersion.binary)
        builder.addField("dependencies", obj.dependencies.toList)
        builder.endObject()
      }
    }
  }
}
