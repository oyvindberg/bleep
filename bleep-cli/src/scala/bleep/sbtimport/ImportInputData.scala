package bleep
package sbtimport

import bleep.internal.{codecs, parseBloopFile, FileUtils, GeneratedFile}
import bleep.nosbt.librarymanagement
import bloop.config.Config
import coursier.core.Configuration
import io.circe.Codec
import io.circe.generic.semiauto.*
import ryddig.Logger

import java.nio.file.{Files, Path}
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.jdk.StreamConverters.StreamHasToScala
import scala.util.{Failure, Success, Try}

/** Absolutely everything that the import cares about from the sbt build is dumped in this structure. It's all read up front, so the import is a pure function.
  *
  * It operates on strings for the most part because
  *   - this structure is serialized into a compressed file for test
  *   - those tests need to translate back and forth between absolute paths and machine-independent templated strings
  */
case class ImportInputData(
    bloopFileStrings: Vector[(Path, String)],
    sbtExportFilePaths: Vector[(Path, String)],
    hasSources: SortedSet[Path],
    generatedFilesBySourceDir: SortedMap[Path, Vector[GeneratedFile]]
) {

  def replace(r: model.Replacements.Replacer, rewriteGeneratedFiles: Boolean): ImportInputData =
    new ImportInputData(
      bloopFileStrings.map { case (p, str) => (r.path(p), r.string(str)) },
      sbtExportFilePaths.map { case (p, str) => (r.path(p), r.string(str)) },
      hasSources.map(r.path),
      generatedFilesBySourceDir
        .map { case (p, gfs) =>
          (
            r.path(p),
            gfs.map(gf =>
              gf.copy(
                isResource = gf.isResource,
                contents = if (rewriteGeneratedFiles) r.string(gf.contents) else gf.contents,
                toRelPath = r.relPath(gf.toRelPath)
              )
            )
          )
        }
    )

  lazy val bloopFiles: Vector[Config.File] =
    bloopFileStrings.map { case (_, contents) => parseBloopFile(contents) }

  lazy val sbtExportFiles: Vector[ReadSbtExportFile.ExportedProject] =
    sbtExportFilePaths.map { case (path, contents) => ReadSbtExportFile.parse(path, contents) }

  lazy val projects: Map[model.CrossProjectName, ImportInputData.InputProject] = {
    val all = bloopFiles
      .collect { case bloopFile =>
        val sbtExportFile =
          sbtExportFiles.filter(f => f.bloopName == bloopFile.project.name).toList match {
            case Nil =>
              sys.error(s"Couldn't pick sbt export file for project ${bloopFile.project.name}")
            case List(one) =>
              one
            case many =>
              many
                .find(f => bloopFile.project.scala.map(_.version).contains(f.scalaVersion.full))
                .orElse(many.find(f => bloopFile.project.scala.map(s => model.VersionScala(s.version).binVersion).contains(f.scalaVersion.binary)))
                .getOrElse {
                  sys.error(s"Couldn't pick sbt export file for project ${bloopFile.project.name} among ${many.map(_.scalaVersion)}")
                }
          }

        ImportInputData.InputProject(bloopFile, sbtExportFile)
      }

    val cross = all
      .groupBy(ip => ip.bleepProjectName)
      .flatMap {
        case (name, one) if one.size == 1 =>
          List((model.CrossProjectName(name, None), one.head))
        case (name, importableProjects) =>
          // multiple projects may translate to same crossId. we discard all except one
          val byCrossId: Map[Option[model.CrossId], Iterable[ImportInputData.InputProject]] =
            importableProjects.groupBy(ip => ImportInputData.mkCrossId(ip, None))

          byCrossId.map {
            case (maybeCrossId, one) if one.size == 1 =>
              (model.CrossProjectName(name, maybeCrossId), one.head)
            case (maybeCrossId, conflicting) =>
              // note that this being a `Map` the discard would be automatic,
              // but for consistency this code will pick the same each time
              val chosen = conflicting.maxBy(ip => ImportInputData.mkCrossId(ip, overrideIsFull = Some(true)))
              (model.CrossProjectName(name, maybeCrossId), chosen)
          }
      }

    ImportInputData.keepRelevant(cross, hasSources)
  }

  lazy val byBloopName: Map[String, model.CrossProjectName] =
    projects.map { case (crossName, ip) => (ip.bloopName, crossName) }

  lazy val generatedFiles: Map[model.CrossProjectName, Vector[GeneratedFile]] =
    projects.map { case (crossName, inputProject) =>
      val fromSources = inputProject.bloopFile.project.sources.flatMap(sourceDir => generatedFilesBySourceDir.getOrElse(sourceDir, Nil))
      val fromResources = inputProject.bloopFile.project.resources.getOrElse(Nil).flatMap(sourceDir => generatedFilesBySourceDir.getOrElse(sourceDir, Nil))
      val generated = fromSources.toVector ++ fromResources

      (crossName, generated)
    }
}

object ImportInputData {
  import codecs.*
  codecPath.discard()
  implicit val codec: Codec.AsObject[ImportInputData] = deriveCodec

  def collectFromFileSystem(
      destinationPaths: BuildPaths,
      logger: Logger
  ): ImportInputData = {
    val bloopFileStrings: Vector[(Path, String)] =
      findGeneratedJsonFiles(destinationPaths.bleepImportBloopDir)
        .map(path => (path, Files.readString(path)))

    // we'll re-parse these later. it's inefficient, but simplifies test harness
    // - need to parse them here to peek into source directories and so on
    // - need to persist them as strings for tests
    val bloopFiles: Vector[Config.File] =
      bloopFileStrings.map { case (_, contents) => parseBloopFile(contents) }

    val sbtExportFileStrings: Vector[(Path, String)] =
      findGeneratedJsonFiles(destinationPaths.bleepImportSbtExportDir)
        .map(path => (path, Files.readString(path)))

    val hasSources: SortedSet[Path] = {
      def isSource(path: Path): Boolean =
        path.toString match {
          case p if p.endsWith(".scala") => true
          case p if p.endsWith(".java")  => true
          case _                         => false
        }

      bloopFiles.iterator
        .flatMap(_.project.sources)
        .to(SortedSet.evidenceIterableFactory)
        .filter(path => Files.exists(path) && Files.walk(path).filter(isSource).findFirst().isPresent)
    }

    val generatedFilesBySourceFolder: SortedMap[Path, Vector[GeneratedFile]] =
      bloopFiles
        .flatMap { file =>
          val bloopProject = file.project
          val originalTargetDir = findOriginalTargetDir(bloopProject).getOrElse {
            sys.error(s"couldn't find target directory for bloop project ${bloopProject.name}")
          }

          def findFiles(dirs: List[Path], isResource: Boolean): List[(Path, GeneratedFile)] =
            for {
              dir <- dirs
              if dir.startsWith(originalTargetDir) && FileUtils.exists(dir)
              file <- Files.walk(dir).filter(Files.isRegularFile(_)).iterator().asScala
              // reading non UTF-8 files will fail, but that's fine
              content <- Try(Files.readString(file)) match {
                case Failure(exception) => logger.warn(s"Failed to read $file: $exception"); None
                case Success(value)     => Some(value)
              }
            } yield (dir, GeneratedFile(isResource = isResource, content, RelPath.relativeTo(dir, file)))

          val sources = findFiles(bloopProject.sources, isResource = false)
          val resources = findFiles(bloopProject.resources.getOrElse(Nil), isResource = true)
          sources ++ resources
        }
        .groupMap { case (path, _) => path } { case (_, gen) => gen }
        .to(SortedMap.sortedMapFactory)

    new ImportInputData(bloopFileStrings, sbtExportFileStrings, hasSources, generatedFilesBySourceFolder)
  }

  def findGeneratedJsonFiles(under: Path): Vector[Path] =
    Files
      .list(under)
      .filter(Files.isDirectory(_))
      .flatMap(dir => Files.list(dir).filter(x => Files.isRegularFile(x) && x.getFileName.toString.endsWith(".json")))
      .toScala(Vector)

  case class InputProject(bloopFile: Config.File, sbtExportFile: ReadSbtExportFile.ExportedProject) {
    val projectType = ProjectType.of(bloopFile.project.name)
    val sbtName: String = sbtExportFile.sbtName
    val bloopName: String = bloopFile.project.name
    val bleepProjectName: model.ProjectName = {
      val suffix: String = projectType match {
        case ProjectType.Main => ""
        case ProjectType.Test => "-test"
        case ProjectType.It   => "-it"
      }

      if (sbtName.endsWith("-test"))
        model.ProjectName(sbtName.replace("-test", "+test") + suffix)
      else if (sbtName.endsWith("-it"))
        model.ProjectName(sbtName.replace("-it", "+it") + suffix)
      else model.ProjectName(sbtName + suffix)
    }
  }

  sealed abstract class ProjectType(val configuration: Configuration, val sbtScope: String, val testLike: Boolean)

  object ProjectType {
    case object Main extends ProjectType(Configuration.empty, "main", testLike = false)
    case object Test extends ProjectType(Configuration.test, "test", testLike = true)
    case object It extends ProjectType(Configuration("it"), "it", testLike = true)

    def of(bloopName: String): ProjectType =
      if (bloopName.endsWith("-test")) Test
      else if (bloopName.endsWith("-it")) It
      else Main
  }

  def keepRelevant(
      projects: Map[model.CrossProjectName, ImportInputData.InputProject],
      hasSources: Path => Boolean
  ): Map[model.CrossProjectName, InputProject] = {
    // not transitive
    val reverseBloopDeps: Map[String, Iterable[String]] =
      projects
        .flatMap { case (_, f) => f.bloopFile.project.dependencies.map(from => (from, f.bloopFile.project.name)) }
        .groupBy { case (from, _) => from }
        .map { case (name, tuples) => (name, tuples.map { case (_, to) => to }.toSet) }

    // may have multiple cross builds per project name. we'll consider them together
    val byName: Map[String, Map[model.CrossProjectName, InputProject]] =
      projects.groupBy { case (_, f) => f.bloopFile.project.name }

    def include(projectName: String): Boolean = {
      val keepBecauseFoundSources = byName(projectName).exists { case (_, f) => f.bloopFile.project.sources.exists(hasSources.apply) }
      def keepBecauseFoundDownstreamWithSources = reverseBloopDeps.getOrElse(projectName, Nil).exists(include)
      keepBecauseFoundSources || keepBecauseFoundDownstreamWithSources
    }

    projects.filter { case (_, f) => include(f.bloopFile.project.name) }
  }

  def mkCrossId(ip: ImportInputData.InputProject, overrideIsFull: Option[Boolean]): Option[model.CrossId] =
    model.CrossId.defaultFrom(
      maybeScalaVersion = ip.bloopFile.project.scala.map(s => model.VersionScala(s.version)),
      maybePlatformId = ip.bloopFile.project.platform.flatMap(p => model.PlatformId.fromName(p.name)),
      isFull = overrideIsFull.getOrElse {
        ip.sbtExportFile.crossVersion match {
          case _: librarymanagement.CrossVersion.Full => true
          case _                                      => false
        }
      }
    )
}
