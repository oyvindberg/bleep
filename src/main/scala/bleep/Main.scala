package bleep

import bloop.config.ConfigCodecs
import com.github.plokhotnyuk.jsoniter_scala.core.{WriterConfig, writeToString}

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.concurrent.ExecutionContext

object Main extends App {
  val pwd = new File(System.getProperty("user.dir"))

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

  val resolver = new CoursierResolver(ExecutionContext.global, downloadSources = true)
  val workspaceDir = pwd.toPath
  val bloopFilesDir = workspaceDir / Defaults.BloopFolder
  val parsedProject = parseProjectFile(Files.readString(buildFile))
  val bloopFiles = generateBloopFiles(parsedProject, workspaceDir, resolver)

  bloopFiles.foreach { p =>
    val json = writeToString(p, WriterConfig.withIndentionStep(2))(ConfigCodecs.codecFile)
    Files.createDirectories(bloopFilesDir)
    val toPath = bloopFilesDir / (p.project.name + ".json")
    Files.write(toPath, json.getBytes(StandardCharsets.UTF_8))
  }

  println(s"Wrote ${bloopFiles.size} files to $bloopFilesDir")
}
