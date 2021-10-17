package bleep

import bleep.internal.Lazy
import bleep.model.ScriptName
import bloop.config.{ConfigCodecs, Config => b}
import com.github.plokhotnyuk.jsoniter_scala

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import scala.concurrent.ExecutionContext
import scala.util.Try

object Main {
  val cwd: Path = Paths.get(System.getProperty("user.dir"))

  // keep looking up until we find build file
  val buildFile: Path = {
    def is(f: File): Option[File] =
      Option(f.list()) match {
        case Some(list) if list.contains(Defaults.BuildFileName) => Some(new File(f, Defaults.BuildFileName))
        case _                                                   => None
      }

    def search(f: File): Option[File] =
      is(f).orElse(Option(f.getParentFile).flatMap(search))

    search(cwd.toFile).getOrElse(sys.error(s"Couldn't find ${Defaults.BuildFileName} from $cwd")).toPath
  }

  val projectDir = buildFile.getParent
  val bloopFilesDir = projectDir / Defaults.BloopFolder

  def ensureBloopFilesUpToDate(parsedProject: model.File): Lazy[List[b.File]] = {
    val hashFile = bloopFilesDir / ".digest"
    val currentHash = parsedProject.toString.hashCode().toString // todo: unstable hash
    val oldHash = Try(Files.readString(hashFile, UTF_8)).toOption

    val lazyBloopFiles: Lazy[List[b.File]] = Lazy {
      val resolver = new CoursierResolver(ExecutionContext.global, downloadSources = true)
      generateBloopFiles(parsedProject, cwd, resolver)
    }

    if (oldHash.contains(currentHash)) {
      println(s"$bloopFilesDir up to date")
    } else {
      val bloopFiles = lazyBloopFiles.forceGet()
      bloopFiles.foreach { p =>
        val json = jsoniter_scala.core.writeToString(p, jsoniter_scala.core.WriterConfig.withIndentionStep(2))(ConfigCodecs.codecFile)
        Files.createDirectories(bloopFilesDir)
        val toPath = bloopFilesDir / (p.project.name + ".json")
        Files.writeString(toPath, json, UTF_8)
      }
      Files.writeString(hashFile, currentHash, UTF_8)
      println(s"Wrote ${bloopFiles.size} files to $bloopFilesDir")
    }

    lazyBloopFiles
  }

  def cli(cmd: String): Unit =
    sys.process.Process(cmd).! match {
      case 0 => ()
      case n =>
        System.err.println(s"FAILED: $cmd")
        System.exit(n)
    }

  def main(args: Array[String]): Unit = {
    val parsedProject: model.File = parseProjectFile(Files.readString(buildFile))
    val lazyBloopFiles = ensureBloopFilesUpToDate(parsedProject)

    args.toList match {
      case List("compile") =>
        cli(s"bloop compile ${parsedProject.projects.keys.map(_.value).mkString(" ")}")
      case List("test") =>
        cli(s"bloop test ${parsedProject.projects.keys.map(_.value).mkString(" ")}")
      case head :: rest if parsedProject.scripts.exists(_.contains(ScriptName(head))) =>
        val scriptDefs = parsedProject.scripts.get(ScriptName(head))
        cli(s"bloop compile ${scriptDefs.values.map(_.project.value).distinct.mkString(" ")}")

        scriptDefs.values.foreach { scriptDef =>
          val bloopProject = lazyBloopFiles.forceGet().find(_.project.name == scriptDef.project.value).get

          val projectClassPath =
            //  todo: bloop doesn't put the files where we want it to
            bloopFilesDir.resolve(scriptDef.project.value).resolve("bloop-bsp-clients-classes/classes-bloop-cli")

          val fullClassPath = List(
            List(projectClassPath),
            bloopProject.project.resources.getOrElse(Nil),
            bloopProject.project.classpath
          ).flatten

          cli(s"java -cp ${fullClassPath.mkString(":")} ${scriptDef.main} ${rest.mkString(" ")}")
        }
      case _ =>
        cli(s"bloop ${args.mkString(" ")}")
    }
  }
}
