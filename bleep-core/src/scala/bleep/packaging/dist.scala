package bleep
package packaging

import bleep.internal.rewriteDependentData

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.{PosixFilePermission, PosixFilePermissions}
import java.nio.file.{Files, Path}
import java.util
import scala.collection.immutable.SortedMap

object dist {
  private val executablePermissions = PosixFilePermissions.fromString("rwxrwxr-x")

  final case class Program(name: String, mainClass: String)

  def apply(started: Started, crossName: model.CrossProjectName, programs: List[Program], overridePath: Option[Path]): Unit = {
    val project = started.build.explodedProjects(crossName)
    val projectPaths = started.buildPaths.project(crossName, project)
    val bloopProject = started.bloopFiles(crossName).forceGet.project

    val fromBuild: SortedMap[RelPath, Array[Byte]] =
      rewriteDependentData(started.build.explodedProjects)
        .startFrom[Array[Byte]](Set(crossName)) { case (crossName, p, eval) =>
          // evaluate dependencies for side effect, the evaluation will be picked up in `startFrom`
          started.build.resolvedDependsOn(crossName).foreach(crossName => eval(crossName).forceGet)

          val projectPaths = started.buildPaths.project(crossName, p)
          createJar(
            JarType.Jar,
            ManifestCreator.default,
            projectPaths.resourcesDirs.all + projectPaths.classes,
            Some(crossName),
            p.platform.flatMap(_.mainClass)
          )
        }
        .map { case (crossName, bytes) => (RelPath.force(s"lib/${crossName.value}.jar"), bytes) }

    val fromResolvedExternal = bloopProject.classpath.collect {
      case path if Files.isRegularFile(path) => (RelPath.force(s"lib/${path.getFileName}"), Files.readAllBytes(path))
    }

    val fromPrograms: List[(RelPath, (String, Option[util.Set[PosixFilePermission]]))] =
      programs.flatMap { program =>
        Map(
          (
            RelPath.force(s"bin/${program.name}"),
            (distShScript(jvmOptions = "", mainClass = program.mainClass), Some(executablePermissions))
          ),
          (
            RelPath.force(s"bin/${program.name}.bat"),
            (distBatScript("", program.mainClass), None)
          )
        )
      }

    val all: Map[RelPath, Array[Byte]] =
      fromBuild ++
        fromResolvedExternal ++
        fromPrograms.map { case (relPath, (str, _)) => (relPath, str.getBytes(StandardCharsets.UTF_8)) }

    val distDir = overridePath.getOrElse(projectPaths.targetDir / "dist")

    FileSync
      .syncBytes(distDir, all, deleteUnknowns = FileSync.DeleteUnknowns.Yes(maxDepth = None), soft = true)
      .log(started.logger, s"created distribution in $distDir")

    fromPrograms.foreach {
      case (relPath, (_, Some(permissions))) =>
        Files.setPosixFilePermissions(distDir / relPath, permissions)
      case _ => ()
    }

    started.logger.withContext("distDir", distDir).info("dist complete")
    ()
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
       |exec java $JAVA_OPTS -cp "$APP_CLASSPATH" -Dapp.base="$APP_BASE" -Dapp.home="$APP_HOME" @@mainClass@@ "$@"
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
