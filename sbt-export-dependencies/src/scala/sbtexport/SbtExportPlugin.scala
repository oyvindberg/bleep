package sbtexport

import sbt.*
import sbt.Keys.*
import sbt.librarymanagement.{ModuleID, ScalaVersion}
import sbt.plugins.IvyPlugin
import sbt.util.Level
import sjsonnew.support.scalajson.unsafe.{Converter, PrettyPrinter}
import sjsonnew.{Builder, JsonFormat, Unbuilder}

object SbtExportPlugin extends AutoPlugin {

  override def trigger = AllRequirements

  override def requires = IvyPlugin

  object autoImport {
    // @transient: sbt 2 caches task results, and a task which writes files is not one to cache. sbt 1 ignores it
    @transient
    val exportAllProjects = taskKey[Seq[File]]("Export all build dependencies")
    @transient
    val exportProject = taskKey[File]("Export build dependencies")
    @transient
    val exportProjectDefinition = taskKey[ExportedProject]("Create build definition")
    val exportProjectsTo = settingKey[File]("Directory to export files to")
  }

  import autoImport.*

  /** Keep a map of all the project names registered in this build load.
    *
    * This map is populated in [[bloopInstall]] before [[bloopGenerate]] is run, which means that by the time [[projectNameFromString]] runs this map will
    * already contain an updated list of all the projects in the build.
    *
    * This information is paramount so that we don't generate a project with the same name of a valid user-facing project. For example, if a user defines two
    * projects named `foo` and `foo-test`, we need to make sure that the test configuration for `foo`, mapped to `foo-test` does not collide with the compile
    * configuration of `foo-test`.
    */
  private final val allProjectNames = new scala.collection.mutable.HashSet[String]()

  /** Cache any replacement that has happened to a project name. */
  private final val projectNameReplacements =
    new java.util.concurrent.ConcurrentHashMap[String, String]()

  def projectNameFromString(name: String, configuration: Configuration, logger: Logger): String =
    if (configuration == Compile) name
    else {
      val supposedName = s"$name-${configuration.name}"
      // Let's check if the default name (with no append of anything) is not used by another project
      if (!allProjectNames.contains(supposedName)) supposedName
      else {
        val existingReplacement = projectNameReplacements.get(supposedName)
        if (existingReplacement != null) existingReplacement
        else {
          // Use `+` instead of `-` as separator and report the user about the change
          val newUnambiguousName = s"$name+${configuration.name}"
          projectNameReplacements.computeIfAbsent(
            supposedName,
            new java.util.function.Function[String, String] {
              override def apply(supposedName: String): String = {
                logger.warn(
                  s"Derived target name '${supposedName}' already exists in the build, changing to ${newUnambiguousName}"
                )
                newUnambiguousName
              }
            }
          )
        }
      }
    }

  val defaultProjectDef = Def.task {
    val org = organization.value
    val project = thisProject.value
    val bloopName = projectNameFromString(project.id, Keys.configuration.value, Keys.streams.value.log)
    val sbtName = moduleName.value
    val scala = Keys.scalaVersion.value
    val binary = Keys.scalaBinaryVersion.value
    val autoScalaLibrary = Keys.autoScalaLibrary.value
    val excludeDependencies = Keys.excludeDependencies.value
    val crossVersion = Keys.crossVersion.value
    // this is essentially libraryDependencies, but it's possible to add non-project dependencies directly to this key
    val dependencies = allDependencies.value.filterNot(projectDependencies.value.contains)
    val schemes = libraryDependencySchemes.value
    val evictionLevel = Keys.evictionErrorLevel.value
    // projects of this build this one depends on, with how their configurations map to this one's: `compile->compile;provided->provided` passes on
    // `provided` dependencies, which the classpath bloop exports shows but does not explain
    val projectDeps = project.dependencies.map(dep => ProjectDependency(dep.project.project, dep.configuration))
    // the files sbt compiles from its source generators. A directory they are generated into may hold other files, which bloop's export of the directory
    // includes and sbt does not compile. Evaluating it runs the source generators
    // undefined in a configuration the project does not have, like IntegrationTest, which then has no generated sources
    val managed = Keys.managedSources.?.value.toList.flatten.map(_.getAbsolutePath).sorted
    ExportedProject(
      org,
      bloopName,
      sbtName,
      ScalaVersion(scala, binary),
      dependencies,
      autoScalaLibrary,
      excludeDependencies,
      crossVersion,
      schemes,
      evictionLevel,
      projectDeps,
      managed
    )
  }

  val defaultExportProjectDef = Def.task {
    val build = exportProjectDefinition.value
    val json = Converter.toJsonUnsafe(build)
    val file = exportProjectsTo.value / build.scalaVersion.full / s"${build.bloopName}.json"
    IO.write(file, PrettyPrinter(json))
    streams.value.log.info(s"Wrote $file")
    file
  }

  val defaultExportProjectsAll = Def.taskDyn[Seq[sbt.File]] {
    val filter = sbt.ScopeFilter(
      sbt.inAnyProject,
      sbt.inAnyConfiguration,
      sbt.inTasks(exportProject)
    )
    exportProject.all(filter)
  }

  def configSettings: Seq[Def.Setting[?]] = Seq(
    exportProjectDefinition := defaultProjectDef.value,
    exportProject := defaultExportProjectDef.value
  )

  override def buildSettings: Seq[Def.Setting[?]] =
    Seq(
      exportProjectsTo := (ThisBuild / baseDirectory).value / "sbt-export-dependencies",
      exportAllProjects := defaultExportProjectsAll.value
    )

  override def projectSettings: Seq[Def.Setting[?]] =
    (Seq(Compile, Test) ++ Compat.extraConfigurations).flatMap(config => sbt.inConfig(config)(configSettings))
}

/** A project of the same build this one depends on.
  *
  * @param project
  *   the project's id, which is also its bloop name
  * @param configuration
  *   how its configurations map to the dependent's, `compile->compile;test->test` say. None is sbt's default, `compile->compile`
  */
case class ProjectDependency(project: String, configuration: Option[String])

object ProjectDependency {
  import sbt.librarymanagement.LibraryManagementCodec.{given, *}

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
    managedSources: Seq[String]
)

object ExportedProject {

  import sbt.librarymanagement.LibraryManagementCodec.{given, *}

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
