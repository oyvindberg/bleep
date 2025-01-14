package bleep
package commands

import bleep.internal.BleepTemplateLogger
import bleep.templates.templatesInfer
import cats.data.NonEmptyList
import ryddig.Logger

import java.nio.file.{Files, Path}
import scala.concurrent.ExecutionContext

case class BuildCreateNew(
    logger: Logger,
    userPaths: UserPaths,
    cwd: Path,
    platforms: NonEmptyList[model.PlatformId],
    scalas: NonEmptyList[model.VersionScala],
    name: String,
    bleepVersion: model.BleepVersion,
    coursierResolver: CoursierResolver.Factory
) extends BleepCommand {

  override def run(): Either[BleepException, Unit] = {
    val buildLoader = BuildLoader.inDirectory(cwd)
    val buildPaths = BuildPaths(cwd, buildLoader, model.BuildVariant.Normal)
    generate(buildPaths).map(_ => ())
  }

  def generate(buildPaths: BuildPaths): Either[BleepException, (Started, Map[Path, String])] = {
    val allFiles = genAllFiles(buildPaths)

    FileSync
      .syncPaths(cwd, allFiles, deleteUnknowns = FileSync.DeleteUnknowns.No, soft = true)
      .log(logger, "Wrote build files")

    val ec = ExecutionContext.global
    val pre = Prebootstrapped(logger, userPaths, buildPaths, BuildLoader.Existing(buildPaths.bleepYamlFile), ec)
    val config = BleepConfigOps.loadOrDefault(pre.userPaths).orThrow

    bootstrap.from(pre, GenBloopFiles.SyncToDisk, rewrites = Nil, config, coursierResolver) map { started =>
      val projects = started.bloopFiles.values.toList.map(_.forceGet.project)
      logger.info(s"Created ${projects.length} projects for build")
      val sourceDirs = projects.flatMap(_.sources).distinct
      val resourceDirs = projects.flatMap(_.resources.getOrElse(Nil)).distinct
      sourceDirs.foreach { path =>
        logger.withContext("path", path).debug("Creating source directory")
        Files.createDirectories(path)
      }
      resourceDirs.foreach { path =>
        logger.withContext("path", path).debug("Creating resource directory")
        Files.createDirectories(path)
      }

      (started, allFiles)
    }
  }

  def genAllFiles(buildPaths: BuildPaths): Map[Path, String] = {
    val exampleFiles = new BuildCreateNew.ExampleFiles(name, isCrossLayout = scalas.length > 1 || platforms.length > 1)
    val build = BuildCreateNew.genBuild(logger, exampleFiles, platforms, scalas, name, bleepVersion)

    val value = Map[Path, String](
      buildPaths.bleepYamlFile -> yaml.encodeShortened(build),
      buildPaths.buildDir / exampleFiles.main.relPath -> exampleFiles.main.contents,
      buildPaths.buildDir / exampleFiles.test.relPath -> exampleFiles.test.contents
    )
    value
  }
}

object BuildCreateNew {

  class ExampleFiles(name: String, isCrossLayout: Boolean) {
    val fansi = model.Dep.Scala("com.lihaoyi", "fansi", "0.3.1")
    val shared: String = if (isCrossLayout) "shared/" else ""

    object main {
      val relPath = RelPath.force(s"$name/${shared}src/scala/com/foo/App.scala")
      val contents =
        s"""package com.foo
           |
           |import fansi._
           |
           |object App {
           |  def greeting(name: String) = Str(s"Hello, ") ++ Color.Red(name)
           |
           |  def main(args: Array[String]): Unit = {
           |    println(greeting("World"))
           |  }
           |}
           |""".stripMargin
      val cls = "com.foo.App"
    }

    val scalatest = model.Dep.Scala("org.scalatest", "scalatest", "3.2.13")
    object test {
      val relPath = RelPath.force(s"tests/${shared}src/scala/com/foo/AppTest.scala")
      val contents =
        s"""package com.foo
           |
           |import org.scalactic.TypeCheckedTripleEquals
           |import org.scalatest.funsuite.AnyFunSuite
           |
           |class AppTest extends AnyFunSuite with TypeCheckedTripleEquals {
           |  test("works") {
           |    assert(App.greeting("a").plainText === "Hello, a")
           |  }
           |}
           |""".stripMargin
    }
  }

  def genBuild(
      logger: Logger,
      exampleFiles: ExampleFiles,
      platforms: NonEmptyList[model.PlatformId],
      scalas: NonEmptyList[model.VersionScala],
      name: String,
      bleepVersion: model.BleepVersion
  ): model.BuildFile = {
    val defaultOpts =
      model.Options(Set(model.Options.Opt.WithArgs("-encoding", List("utf8")), model.Options.Opt.Flag("-feature"), model.Options.Opt.Flag("-unchecked")))

    def variants(name: String): NonEmptyList[(model.PlatformId, model.VersionScala, model.CrossProjectName, model.SourceLayout)] =
      if (scalas.size == 1 && platforms.size == 1)
        NonEmptyList((platforms.head, scalas.head, model.CrossProjectName(model.ProjectName(name), None), model.SourceLayout.Normal), Nil)
      else
        for {
          p <- platforms
          s <- scalas
        } yield (
          p,
          s,
          model.CrossProjectName(model.ProjectName(name), model.CrossId.defaultFrom(Some(s), Some(p), isFull = false)),
          model.SourceLayout.CrossFull
        )

    val mainProjects: NonEmptyList[(model.CrossProjectName, model.Project)] =
      variants(name).map { case (platformId, scala, crossName, sourceLayout) =>
        val p = model.Project(
          `extends` = model.JsonSet.empty,
          cross = model.JsonMap.empty,
          folder = None,
          dependsOn = model.JsonSet.empty,
          `source-layout` = Some(sourceLayout),
          `sbt-scope` = None,
          sources = model.JsonSet.empty,
          resources = model.JsonSet.empty,
          dependencies = model.JsonSet(exampleFiles.fansi),
          java = None,
          scala = Some(model.Scala(version = Some(scala), options = defaultOpts, setup = None, compilerPlugins = model.JsonSet.empty, strict = Some(true))),
          platform = Some(
            platformId match {
              case model.PlatformId.Jvm =>
                model.Platform.Jvm(model.Options.empty, jvmMainClass = Some(exampleFiles.main.cls), jvmRuntimeOptions = model.Options.empty)
              case model.PlatformId.Js =>
                model.Platform
                  .Js(model.VersionScalaJs.ScalaJs1, None, None, None, None, jsNodeVersion = Some(constants.Node), Some(exampleFiles.main.cls))
              case model.PlatformId.Native =>
                model.Platform
                  .Native(model.VersionScalaNative.ScalaNative04, Some("immix"), Some(exampleFiles.main.cls), None, None, None, None, None, None, None)
            }
          ),
          isTestProject = None,
          testFrameworks = model.JsonSet.empty,
          sourcegen = model.JsonSet.empty,
          libraryVersionSchemes = model.JsonSet.empty
        )
        (crossName, p)
      }

    val testProjects: NonEmptyList[(model.CrossProjectName, model.Project)] =
      variants("tests").map { case (platformId, scala, crossName, sourceLayout) =>
        val p = model.Project(
          `extends` = model.JsonSet.empty,
          cross = model.JsonMap.empty,
          folder = None,
          dependsOn = model.JsonSet(model.ProjectName(name)),
          `source-layout` = Some(sourceLayout),
          `sbt-scope` = None,
          sources = model.JsonSet.empty,
          resources = model.JsonSet.empty,
          dependencies = model.JsonSet(exampleFiles.scalatest),
          java = None,
          scala = Some(model.Scala(version = Some(scala), options = defaultOpts, setup = None, compilerPlugins = model.JsonSet.empty, strict = Some(true))),
          platform = Some(
            platformId match {
              case model.PlatformId.Jvm => model.Platform.Jvm(model.Options.empty, jvmMainClass = None, jvmRuntimeOptions = model.Options.empty)
              case model.PlatformId.Js  => model.Platform.Js(model.VersionScalaJs.ScalaJs1, None, None, None, None, Some(constants.Node), None)
              case model.PlatformId.Native =>
                model.Platform.Native(model.VersionScalaNative.ScalaNative04, Some("immix"), None, None, None, None, None, None, None, None)
            }
          ),
          isTestProject = Some(true),
          testFrameworks = model.JsonSet.empty,
          sourcegen = model.JsonSet.empty,
          libraryVersionSchemes = model.JsonSet.empty
        )
        (crossName, p)
      }

    val explodedBuild = model.Build.Exploded(
      bleepVersion.latestRelease,
      explodedProjects = (mainProjects.toList ++ testProjects.toList).toMap,
      resolvers = model.JsonList.empty,
      jvm = Some(model.Jvm.graalvm),
      scripts = Map.empty
    )

    templatesInfer(new BleepTemplateLogger(logger), explodedBuild, ignoreWhenInferringTemplates = _ => false)
  }
}
