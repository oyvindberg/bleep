package bleep.sbtexport

import bleep.sbtexport.Lm.*
import sjsonnew.BasicJsonProtocol.*
import sjsonnew.{Builder, JsonFormat, Unbuilder}

/** What sbt-export-dependencies writes for each project of the build it runs in, and what `bleep import-sbt` reads.
  *
  * One source for both sides. The plugin compiles it against sbt's own librarymanagement, for sbt 1 and sbt 2; bleep compiles it against its copy of it,
  * `bleep.nosbt.librarymanagement`. Each side has an `Lm` object naming the types and their JSON formats.
  *
  * @param bloopName
  *   the name bloop gives the project in this configuration
  * @param managedSources
  *   the files sbt compiles from its source generators, absolute. A directory they are generated into may hold other files, which bloop's export of the
  *   directory includes and sbt does not compile
  */
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
    evictionErrorLevel: Level,
    projectDependencies: Seq[ProjectDependency],
    managedSources: Seq[String]
)

object ExportedProject {
  implicit val levelFormat: JsonFormat[Level] = new JsonFormat[Level] {
    override def read[J](jsOpt: Option[J], unbuilder: Unbuilder[J]): Level =
      jsOpt match {
        case Some(j) =>
          val levelString = unbuilder.readString(j)
          Levels.values.find(_.toString == levelString) match {
            case Some(level) => level
            case None        => sjsonnew.deserializationError(s"Unknown level: $levelString")
          }
        case None =>
          sjsonnew.deserializationError("expected a level value")
      }

    override def write[J](obj: Level, builder: Builder[J]): Unit =
      builder.writeString(obj.toString)
  }

  implicit val format: JsonFormat[ExportedProject] = new JsonFormat[ExportedProject] {
    override def read[J](jsOpt: Option[J], unbuilder: Unbuilder[J]): ExportedProject =
      jsOpt match {
        case Some(j) =>
          val _ = unbuilder.beginObject(j)
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
          val evictionErrorLevel = unbuilder.readField[Level]("evictionErrorLevel")
          val projectDependencies = unbuilder.readField[Seq[ProjectDependency]]("projectDependencies")
          val managedSources = unbuilder.readField[Seq[String]]("managedSources")
          unbuilder.endObject()

          ExportedProject(
            organization,
            bloopName,
            sbtName,
            scalaVersion(scalaFullVersion, scalaBinaryVersion),
            dependencies,
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

/** A project of the same build this one depends on.
  *
  * @param project
  *   the project's id, which is also its bloop name
  * @param configuration
  *   how its configurations map to the dependent's, `compile->compile;provided->provided` say. None is sbt's default, `compile->compile`
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
  implicit val format: JsonFormat[ProjectDependency] = new JsonFormat[ProjectDependency] {
    override def read[J](jsOpt: Option[J], unbuilder: Unbuilder[J]): ProjectDependency =
      jsOpt match {
        case Some(j) =>
          val _ = unbuilder.beginObject(j)
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
