package bleep

import bloop.config.ConfigCodecs
import com.github.plokhotnyuk.jsoniter_scala.core.{writeToString, WriterConfig}

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.concurrent.ExecutionContext
import scala.util.Try

object Main {
  val pwd = new File(System.getProperty("user.dir"))
  val workspaceDir = pwd.toPath

  // keep looking up until we find build file
  val buildFile: Path = {
    def is(f: File): Option[File] =
      Option(f.list()) match {
        case Some(list) if list.contains(Defaults.BuildFileName) => Some(new File(f, Defaults.BuildFileName))
        case _                                                   => None
      }

    def search(f: File): Option[File] =
      is(f).orElse(Option(f.getParentFile).flatMap(search))

    search(pwd).getOrElse(sys.error(s"Couldn't find ${Defaults.BuildFileName} from $pwd")).toPath
  }

  def ensureBloopFilesUpToDate(parsedProject: model.File): Unit = {
    val bloopFilesDir = workspaceDir / Defaults.BloopFolder
    val hashFile = bloopFilesDir / ".digest"
    val currentHash = parsedProject.toString.hashCode().toString // todo: unstable hash
    val oldHash = Try(Files.readString(hashFile, UTF_8)).toOption
    if (oldHash.contains(currentHash)) {
      println(s"$bloopFilesDir up to date")
    } else {
      val resolver = new CoursierResolver(ExecutionContext.global, downloadSources = true)
      val bloopFiles = generateBloopFiles(parsedProject, workspaceDir, resolver)

      bloopFiles.foreach { p =>
        val json = writeToString(p, WriterConfig.withIndentionStep(2))(ConfigCodecs.codecFile)
        Files.createDirectories(bloopFilesDir)
        val toPath = bloopFilesDir / (p.project.name + ".json")
        Files.writeString(toPath, json, UTF_8)
      }
      Files.writeString(hashFile, currentHash, UTF_8)
      println(s"Wrote ${bloopFiles.size} files to $bloopFilesDir")
    }
  }

  def main(args: Array[String]): Unit = {
    val parsedProject: model.File = parseProjectFile(Files.readString(buildFile))
    ensureBloopFilesUpToDate(parsedProject)

    args match {
      case Array("compile") =>
        sys.process.Process(s"bloop compile ${parsedProject.projects.keys.map(_.value).mkString(" ")}").run()
      case Array("test") =>
        sys.process.Process(s"bloop test ${parsedProject.projects.keys.map(_.value).mkString(" ")}").run()
      case Array() =>
    }
  }
}
