package bleep

import bleep.internal.Lazy
import bleep.model.ScriptName
import bloop.config.{ConfigCodecs, Config => b}
import com.github.plokhotnyuk.jsoniter_scala
import io.circe.syntax._

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import scala.collection.immutable.SortedMap
import scala.concurrent.ExecutionContext
import scala.util.Try

object Main {
  implicit val cwd: Path = Paths.get(System.getProperty("user.dir"))

  // keep looking up until we find build file
  def findBleepJson: Either[String, Path] = {
    def is(f: File): Option[File] =
      Option(f.list()) match {
        case Some(list) if list.contains(Defaults.BuildFileName) => Some(new File(f, Defaults.BuildFileName))
        case _                                                   => None
      }

    def search(f: File): Option[File] =
      is(f).orElse(Option(f.getParentFile).flatMap(search))

    search(cwd.toFile) match {
      case Some(found) => Right(found.toPath)
      case None        => Left(s"Couldn't find ${Defaults.BuildFileName} from $cwd")
    }
  }

  def ensureBloopFilesUpToDate(buildDir: Path, parsedProject: model.File): Map[model.ProjectName, Lazy[b.File]] = {
    val bloopFilesDir = buildDir / Defaults.BloopFolder
    val hashFile = bloopFilesDir / ".digest"
    val currentHash = parsedProject.toString.hashCode().toString // todo: unstable hash
    val oldHash = Try(Files.readString(hashFile, UTF_8)).toOption

    if (oldHash.contains(currentHash)) {
      println(s"$bloopFilesDir up to date")

      parsedProject.projects.map { case (projectName, _) =>
        val load = Lazy(readBloopFile(bloopFilesDir, projectName))
        (projectName, load)
      }

    } else {
      val bloopFiles: SortedMap[model.ProjectName, Lazy[b.File]] = {
        val resolver = new CoursierResolver(ExecutionContext.global, downloadSources = true)
        generateBloopFiles(parsedProject, cwd, resolver)
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
      bloopFiles
    }
  }

  def main(args: Array[String]): Unit =
    args.toList match {
      case List("import") =>
        val file = importBloopFilesFromSbt(cwd)
        Files.writeString(
          cwd / Defaults.BuildFileName,
          file.asJson.deepDropNullValues.spaces2,
          UTF_8
        )

//        cli("mv .bloop .bloop_imported")

      case args =>
        findBleepJson match {
          case Left(err) => sys.error(err)
          case Right(buildPath) =>
            val buildDirPath = buildPath.getParent
            val build = model.parseFile(Files.readString(buildPath)) match {
              case Left(error) => throw error
              case Right(file) => file
            }
            val bloopProjects = ensureBloopFilesUpToDate(buildDirPath, build)

            args match {
              case List("compile") =>
                cli(s"bloop compile ${build.projects.keys.map(_.value).mkString(" ")}")
              case List("test") =>
                cli(s"bloop test ${build.projects.keys.map(_.value).mkString(" ")}")
              case head :: rest if build.scripts.exists(_.contains(ScriptName(head))) =>
                val scriptDefs = build.scripts.get(ScriptName(head))
                cli(s"bloop compile ${scriptDefs.values.map(_.project.value).distinct.mkString(" ")}")

                scriptDefs.values.foreach { scriptDef =>
                  val bloopFile = bloopProjects(scriptDef.project).forceGet(scriptDef.project.value)
                  val fullClassPath = fixedClasspath(bloopFile.project)

                  cli(s"java -cp ${fullClassPath.mkString(":")} ${scriptDef.main} ${rest.mkString(" ")}")
                }
              case _ =>
                cli(s"bloop ${args.mkString(" ")}")
            }
        }
    }
}
