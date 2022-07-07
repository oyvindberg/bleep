package bleep.packaging

import bleep.internal.FileUtils
import bleep.logging.Logger
import bloop.config.Config

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path, StandardCopyOption, StandardOpenOption}

object DistPlugin {
  private val standardPermissions = PosixFilePermissions.fromString("rwxrwxr-x")

  final case class Program(name: String, mainClass: String)

  def dist(logger: Logger, projects: List[Config.Project], project: String, programs: List[Program], distPath: Option[Path]): Unit = {
    val dependencyLookup = projects.map(p => p.classesDir -> p).toMap

    projects.find(_.name == project).foreach { project =>
      val distDir = distPath.map(_.resolve(project.name)).getOrElse(project.out.resolve("dist"))
      Files.createDirectories(distDir)
      val lib = distDir.resolve("lib")
      FileUtils.deleteDirectory(lib)
      Files.createDirectories(lib)

      val jarFiles = dependenciesFor(logger, project, dependencyLookup).distinct

      jarFiles.foreach { src =>
        Files.copy(src, lib.resolve(src.getFileName), StandardCopyOption.COPY_ATTRIBUTES)
      }
      if (programs.nonEmpty) {
        val bin = distDir.resolve("bin")
        FileUtils.deleteDirectory(bin)
        Files.createDirectories(bin)
        writeScripts(bin, "", programs)
      }

      logger.withContext(distDir).info("dist complete")
    }
    ()
  }

  def dependenciesFor(logger: Logger, project: Config.Project, lookup: Map[Path, Config.Project]): List[Path] = {
    val (dirs, jars) = project.classpath.partition(Files.isDirectory(_))
    val jar = PackagePlugin.jar(logger, project)
    jar :: jars ::: dirs
      .flatMap(dir => lookup.get(dir).toList)
      .flatMap(dependantProject => dependenciesFor(logger, dependantProject, lookup))
  }

  def writeScripts(to: Path, jvmOptions: String, programs: List[Program]): Unit =
    programs.foreach { program =>
      val bashFile = to.resolve(program.name)
      Files.writeString(
        bashFile,
        distShScript(jvmOptions, program.mainClass),
        StandardCharsets.UTF_8,
        StandardOpenOption.CREATE_NEW
      )
      Files.setPosixFilePermissions(bashFile, standardPermissions)
      Files.writeString(
        to.resolve(s"${program.name}.bat"),
        distBatScript(jvmOptions, program.mainClass),
        StandardCharsets.UTF_8,
        StandardOpenOption.CREATE_NEW
      )
    }

  private def distShScript(jvmOptions: String, mainClass: String): String =
    """|#!/bin/sh
       |
       |PRG="$0"
       |
       |# need this for relative symlinks
       |while [ -h "$PRG" ] ; do
       |  ls=`ls -ld "$PRG"`
       |  link=`expr "$ls" : '.*-> \(.*\)$'`
       |  if expr "$link" : '/.*' > /dev/null; then
       |    PRG="$link"
       |  else
       |    PRG="`dirname "$PRG"`/$link"
       |  fi
       |done
       |
       |APP_BASE=`dirname "$PRG"`/..
       |
       |# make it fully qualified
       |APP_BASE=`cd "$APP_BASE" && pwd`
       |
       |if [ "Z${APP_HOME}" = "Z" ]; then
       |  APP_HOME=$APP_BASE
       |fi
       |
       |APP_CLASSPATH="$APP_BASE/lib/*"
       |JAVA_OPTS="$JAVA_OPTS @@jvmOptions@@"
       |
       |exec java $JAVA_OPTS -cp "$APP_CLASSPATH" -Dapp.base="$APP_BASE" -Dapp.home="$APP_HOME" @@mainClass@@ $@
       |""".stripMargin.replace("@@jvmOptions@@", jvmOptions).replace("@@mainClass@@", mainClass)

  private def distBatScript(jvmOptions: String, mainClass: String): String =
    """|@echo off
       |set APP_HOME=%%~dp0..
       |set APP_CLASSPATH=%%APP_HOME%%\lib\*
       |set JAVA_OPTS=%%JAVA_OPTS%% @@jvmOptions@@
       |set CMD_LINE_ARGS=
       |:setArgs
       |if %1"=="" goto doneSetArgs
       |  set CMD_LINE_ARGS="%%CMD_LINE_ARGS%% %1"
       |  shift
       |  goto setArgs
       |:doneSetArgs
       |
       |java %%JAVA_OPTS%% -cp "%%APP_CLASSPATH%%" -Dapp.base="%%APP_HOME%%" -Dapp.home="%%APP_HOME%%"  @@mainClass@@ "%%CMD_LINE_ARGS%%"
       |""".stripMargin.replace("@@jvmOptions@@", jvmOptions).replace("@@mainClass@@", mainClass)

}
