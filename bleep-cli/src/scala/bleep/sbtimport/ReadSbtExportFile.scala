package bleep
package sbtimport

import bleep.nosbt.librarymanagement.syntax.ExclusionRule
import bleep.nosbt.librarymanagement.{CrossVersion, ModuleID, ScalaVersion}
import sjsonnew.support.scalajson.unsafe.{Converter, Parser}
import sjsonnew.{Builder, JsonFormat, Unbuilder}

import java.nio.file.Path
import scala.util.{Failure, Success}

/** copy/pasted from https://github.com/bleep-build/sbt-export-dependencies to avoid sbt dependency and to cross build
  */
object ReadSbtExportFile {
  def parse(path: Path, jsonStr: String): ExportedProject =
    Parser.parseFromString(jsonStr).flatMap(Converter.fromJson[ExportedProject](_)) match {
      case Failure(exception) => throw new BleepException.InvalidJson(path, exception)
      case Success(value)     => value
    }

  case class ExportedProject(
      organization: String,
      bloopName: String,
      sbtName: String,
      scalaVersion: ScalaVersion,
      dependencies: Seq[ModuleID],
      autoScalaLibrary: Boolean,
      excludeDependencies: Seq[ExclusionRule],
      crossVersion: CrossVersion,
      libraryDependencySchemes: Seq[ModuleID]
  )

  object ExportedProject {
    import bleep.nosbt.librarymanagement.LibraryManagementCodec.*

    implicit val format: JsonFormat[ExportedProject] = new JsonFormat[ExportedProject] {
      override def read[J](jsOpt: Option[J], unbuilder: Unbuilder[J]): ExportedProject =
        jsOpt match {
          case Some(j) =>
            unbuilder.beginObject(j).discard()
            val organization = unbuilder.readField[String]("organization")
            val bloopName = unbuilder.readField[String]("bloopName")
            val sbtName = unbuilder.readField[String]("sbtName")
            val scalaFullVersion = unbuilder.readField[String]("scalaFullVersion")
            val scalaBinaryVersion = unbuilder.readField[String]("scalaBinaryVersion")
            val dependencies = unbuilder.readField[Seq[ModuleID]]("dependencies")
            val autoScalaLibrary = unbuilder.readField[Boolean]("autoScalaLibrary")
            val excludeDependencies = unbuilder.readField[Seq[ExclusionRule]]("excludeDependencies")
            val crossVersion = unbuilder.readField[CrossVersion]("crossVersion")
            val libraryDependencySchemes = unbuilder.readField[Seq[ModuleID]]("libraryDependencySchemes")
            unbuilder.endObject()

            ExportedProject(
              organization,
              bloopName,
              sbtName,
              scalaVersion = ScalaVersion(scalaFullVersion, scalaBinaryVersion),
              dependencies = dependencies,
              autoScalaLibrary,
              excludeDependencies,
              crossVersion,
              libraryDependencySchemes
            )
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
        builder.addField("autoScalaLibrary", obj.autoScalaLibrary)
        builder.addField("excludeDependencies", obj.excludeDependencies)
        builder.addField("crossVersion", obj.crossVersion)
        builder.addField("libraryDependencySchemes", obj.libraryDependencySchemes)
        builder.endObject()
      }
    }
  }

}
