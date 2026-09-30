package bleep
package sbtimport

import bleep.nosbt.librarymanagement.syntax.ExclusionRule
import bleep.nosbt.librarymanagement.{CrossVersion, ModuleID, ScalaVersion}
import bleep.nosbt.util.Level
import sjsonnew.support.scalajson.unsafe.{Converter, Parser}
import sjsonnew.{Builder, JsonFormat, Unbuilder}

import java.nio.file.Path
import scala.util.{Failure, Success}

/** copy/pasted from the sbt plugin `sbt-export-dependencies` in this repository, to avoid an sbt dependency and to cross build
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
      libraryDependencySchemes: Seq[ModuleID],
      evictionErrorLevel: Level.Value,
      projectDependencies: Seq[ProjectDependency],
      /** the files sbt compiles from its source generators, absolute */
      managedSources: Seq[String]
  )

  /** A project of the same build this one depends on, and how its configurations map to this one's (`compile->compile;provided->provided`). None is sbt's
    * default, `compile->compile`
    */
  case class ProjectDependency(project: String, configuration: Option[String]) {

    /** `provided->provided`: the other project's `provided` dependencies are this one's too */
    def passesOnProvided: Boolean =
      configuration.exists(_.split(";").exists { mapping =>
        mapping.split("->").map(_.trim) match {
          case Array("provided", to) => to.split(",").exists(_.trim.takeWhile(_ != '(') == "provided")
          case _                     => false
        }
      })
  }

  object ProjectDependency {
    import bleep.nosbt.librarymanagement.LibraryManagementCodec.*

    implicit val format: JsonFormat[ProjectDependency] = new JsonFormat[ProjectDependency] {
      override def read[J](jsOpt: Option[J], unbuilder: Unbuilder[J]): ProjectDependency =
        jsOpt match {
          case Some(j) =>
            unbuilder.beginObject(j): Unit
            val project = unbuilder.readField[String]("project")
            val configuration = unbuilder.readField[Option[String]]("configuration")
            unbuilder.endObject()
            ProjectDependency(project, configuration)
          case None =>
            sjsonnew.deserializationError("expected a json value to read")
        }

      override def write[J](obj: ProjectDependency, builder: Builder[J]): Unit = {
        builder.beginObject()
        builder.addField("project", obj.project)
        builder.addField("configuration", obj.configuration)
        builder.endObject()
      }
    }
  }

  object ExportedProject {

    import bleep.nosbt.librarymanagement.LibraryManagementCodec.*

    implicit val levelFormat: JsonFormat[Level.Value] = new JsonFormat[Level.Value] {
      override def read[J](jsOpt: Option[J], unbuilder: Unbuilder[J]): Level.Value =
        jsOpt match {
          case Some(j) =>
            val levelString = unbuilder.readString(j)
            Level.values
              .find(_.toString == levelString)
              .getOrElse(
                sjsonnew.deserializationError(s"Unknown level: $levelString")
              )
          case None =>
            sjsonnew.deserializationError("expected a level value")
        }

      override def write[J](obj: Level.Value, builder: Builder[J]): Unit =
        builder.writeString(obj.toString)
    }

    implicit val format: JsonFormat[ExportedProject] = new JsonFormat[ExportedProject] {
      override def read[J](jsOpt: Option[J], unbuilder: Unbuilder[J]): ExportedProject =
        jsOpt match {
          case Some(j) =>
            unbuilder.beginObject(j): Unit
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
            val evictionErrorLevel = unbuilder.readField[Level.Value]("evictionErrorLevel")
            val projectDependencies = unbuilder.readField[Seq[ProjectDependency]]("projectDependencies")
            val managedSources = unbuilder.readField[Seq[String]]("managedSources")
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
              libraryDependencySchemes,
              evictionErrorLevel,
              projectDependencies,
              managedSources
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
        builder.addField("evictionErrorLevel", obj.evictionErrorLevel)
        builder.addField("projectDependencies", obj.projectDependencies)
        builder.addField("managedSources", obj.managedSources)
        builder.endObject()
      }
    }
  }
}
