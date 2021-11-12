package bleep

import bleep.bootstrap.Bootstrapped
import bleep.internal.ShortenJson
import bleep.model.ScriptName
import io.circe.syntax._

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

object Main {
  val cwd: Path = Paths.get(System.getProperty("user.dir"))

  def main(args: Array[String]): Unit =
    args.toList match {
      case List("import") =>
        val build = deduplicateBuild(importBloopFilesFromSbt(cwd))
        Files.writeString(
          cwd / Defaults.BuildFileName,
          build.asJson.foldWith(ShortenJson).spaces2,
          UTF_8
        )
        ()

//        cli("mv .bloop .bloop_imported")

      case args =>
        bootstrap.from(cwd) match {
          case Bootstrapped.InvalidJson(e) =>
            throw e
          case Bootstrapped.BuildNotFound =>
            sys.error(s"Couldn't find a bleep build in (parent directories of) $cwd")
          case Bootstrapped.Ok(buildDirPath, build, parsedProjects, activeProject) =>
            implicit val wd: Path = buildDirPath
            activeProject.foreach(p => println(s"active project: ${p.value}"))

            args match {
              case List("compile") =>
                cli(s"bloop compile ${build.projects.keys.map(_.value).mkString(" ")}")
              case List("test") =>
                cli(s"bloop test ${build.projects.keys.map(_.value).mkString(" ")}")
              case head :: rest if build.scripts.exists(_.contains(ScriptName(head))) =>
                val scriptDefs = build.scripts.get(ScriptName(head))
                cli(s"bloop compile ${scriptDefs.values.map(_.project.value).distinct.mkString(" ")}")

                scriptDefs.values.foreach { scriptDef =>
                  val bloopFile = parsedProjects(scriptDef.project).forceGet(scriptDef.project.value)
                  val fullClassPath = fixedClasspath(bloopFile.project)

                  cli(s"java -cp ${fullClassPath.mkString(":")} ${scriptDef.main} ${rest.mkString(" ")}")
                }
              case _ =>
                cli(s"bloop ${args.mkString(" ")}")
            }
        }
    }
}
