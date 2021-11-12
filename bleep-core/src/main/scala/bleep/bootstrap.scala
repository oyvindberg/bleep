package bleep

import bleep.internal.Lazy
import bloop.config.{Config => b, ConfigCodecs}
import com.github.plokhotnyuk.jsoniter_scala
import io.circe
import net.harawata.appdirs.{AppDirs, AppDirsFactory}

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import scala.collection.immutable.SortedMap
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters._
import scala.util.Try

object bootstrap {
  private val appDirs: AppDirs = AppDirsFactory.getInstance()
  private val cacheDir = Paths.get(appDirs.getUserCacheDir("bleep", "1", "com.olvind"))

  sealed trait Bootstrapped

  object Bootstrapped {
    case object BuildNotFound extends Bootstrapped
    case class InvalidJson(e: circe.Error) extends Bootstrapped

    /** @param build
      *   non-exploded variant, at least for now
      * @param bloopFiles
      *   will either all be resolved and written immediately if outdated, or read and parsed on demand
      */
    case class Ok(buildDirPath: Path, build: model.Build, bloopFiles: Map[model.ProjectName, Lazy[b.File]], activeProject: Option[model.ProjectName])
        extends Bootstrapped {

      lazy val projects: List[b.Project] =
        bloopFiles.map {case (_, lazyProject) => lazyProject.forceGet("").project }.toList
    }
  }

  def fromCwd: Bootstrapped =
    from(Paths.get(System.getProperty("user.dir")))

  def from(cwd: Path): Bootstrapped =
    findBleepJson(cwd) match {
      case Some(bleepJsonPath) =>
        model.parseBuild(Files.readString(bleepJsonPath)) match {
          case Left(th) => Bootstrapped.InvalidJson(th)
          case Right(build) =>
            val buildDirPath = bleepJsonPath.getParent

            val bloopFilesDir = buildDirPath / Defaults.BloopFolder
            val hashFile = bloopFilesDir / ".digest"
            val currentHash = build.toString.hashCode().toString // todo: unstable hash
            val oldHash = Try(Files.readString(hashFile, UTF_8)).toOption

            val activeProject: Option[model.ProjectName] = {
              val withRelativeLength =
                build.projects.flatMap { case (name, p) =>
                  val folder = buildDirPath / p.folder.getOrElse(RelPath.force(name.value))
                  val relative = cwd.relativize(folder)
                  if (relative.iterator().asScala.contains("..")) None
                  else Some((name, relative.getNameCount))
                }

              withRelativeLength.values.minOption.flatMap { min =>
                withRelativeLength.filter(_._2 == min).keys.toList match {
                  case one :: Nil => Some(one)
                  case _          => None
                }
              }
            }

            if (oldHash.contains(currentHash)) {
              println(s"$bloopFilesDir up to date")

              val readLazily: Map[model.ProjectName, Lazy[b.File]] = build.projects.map { case (projectName, _) =>
                val load = Lazy(readBloopFile(bloopFilesDir, projectName))
                (projectName, load)
              }
              Bootstrapped.Ok(buildDirPath, build, readLazily, activeProject)

            } else {
              val bloopFiles: SortedMap[model.ProjectName, Lazy[b.File]] = {
                val resolver = CoursierResolver(ExecutionContext.global, downloadSources = true, Some(cacheDir))
                generateBloopFiles(build, buildDirPath, resolver)
              }

              bloopFiles.foreach { case (projectName, lazyP) =>
                val p = lazyP.forceGet(projectName.value)
                val json = jsoniter_scala.core.writeToString(p, jsoniter_scala.core.WriterConfig.withIndentionStep(2))(ConfigCodecs.codecFile)
                Files.createDirectories(bloopFilesDir)
                val toPath = bloopFilesDir / (projectName.value + ".json")
                Files.writeString(toPath, json, UTF_8)
              }
              Files.writeString(hashFile, currentHash, UTF_8)
              println(s"Wrote ${bloopFiles.size} files to $bloopFilesDir")
              Bootstrapped.Ok(buildDirPath, build, bloopFiles, activeProject)
            }
        }
      case None => Bootstrapped.BuildNotFound
    }

  // keep looking up until we find build file
  def findBleepJson(from: Path): Option[Path] = {
    def is(f: File): Option[File] =
      Option(f.list()) match {
        case Some(list) if list.contains(Defaults.BuildFileName) => Some(new File(f, Defaults.BuildFileName))
        case _                                                   => None
      }

    def search(f: File): Option[File] =
      is(f).orElse(Option(f.getParentFile).flatMap(search))

    search(from.toFile).map(_.toPath)
  }
}
