package bleep
package commands

import bleep.internal.{asYamlString, FileUtils, Templates}
import bleep.logging.Logger
import bleep.model._
import bleep.{constants, BleepException}
import cats.data.NonEmptyList

import java.nio.file.{Files, Path}

case class BuildCreateNew(
    logger: Logger,
    cwd: Path,
    platforms: NonEmptyList[model.PlatformId],
    scalas: NonEmptyList[VersionScala],
    name: String,
    bleepVersion: model.Version
) extends BleepCommand {

  override def run(): Either[BleepException, Unit] = {
    val buildLoader = BuildLoader.inDirectory(cwd)
    val buildPaths = BuildPaths(cwd, buildLoader, BuildPaths.Mode.Normal)
    generate(buildPaths).map(_ => ())
  }

  def generate(buildPaths: BuildPaths): Either[BleepException, (Started, Map[Path, String])] = {
    val allFiles = genAllFiles(buildPaths)

    val syncedFiles = FileUtils.syncPaths(cwd, allFiles, deleteUnknowns = FileUtils.DeleteUnknowns.No, soft = true)

    syncedFiles.foreach { case (path, synced) => logger.info(s"Wrote $path ($synced)") }

    val pre = Prebootstrapped(buildPaths, logger, BuildLoader.Existing(buildPaths.bleepYamlFile))
    val bleepConfig = BleepConfig.lazyForceLoad(pre.userPaths)

    bootstrap.from(pre, GenBloopFiles.SyncToDisk, rewrites = Nil, bleepConfig) map { started =>
      val projects = started.bloopProjectsList
      logger.info(s"Created ${projects.length} projects for build")
      val sourceDirs = projects.flatMap(_.sources).distinct
      val resourceDirs = projects.flatMap(_.resources.getOrElse(Nil)).distinct
      sourceDirs.foreach { path =>
        logger.withContext(path).debug("Creating source directory")
        Files.createDirectories(path)
      }
      resourceDirs.foreach { path =>
        logger.withContext(path).debug("Creating resource directory")
        Files.createDirectories(path)
      }

      (started, allFiles)
    }
  }

  def genAllFiles(buildPaths: BuildPaths): Map[Path, String] = {
    val exampleFiles = new BuildCreateNew.ExampleFiles(name)
    val build = BuildCreateNew.genBuild(logger, exampleFiles, platforms, scalas, name, bleepVersion)

    val value = Map[Path, String](
      buildPaths.bleepYamlFile -> asYamlString(build),
      buildPaths.buildDir / exampleFiles.main.relPath -> exampleFiles.main.contents,
      buildPaths.buildDir / exampleFiles.test.relPath -> exampleFiles.test.contents
    )
    value
  }
}

object BuildCreateNew {

  class ExampleFiles(name: String) {
    val fansi = Dep.Scala("com.lihaoyi", "fansi", "0.3.1")
    object main {
      val relPath = RelPath.force(s"$name/src/scala/com/foo/App.scala")
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

    val scalatest = Dep.Scala("org.scalatest", "scalatest", "3.2.11")
    object test {
      val relPath = RelPath.force(s"$name-test/src/scala/com/foo/AppTest.scala")
      val contents =
        s"""package com.foo
           |
           |import org.scalactic.TypeCheckedTripleEquals
           |import org.scalatest.funsuite.AnyFunSuite
           |
           |class AppTest extends AnyFunSuite with TypeCheckedTripleEquals {
           |  test("works") {
           |    assert(App.greeting("a").plainText === "Hello, , a")
           |  }
           |}
           |""".stripMargin
    }
  }

  def genBuild(
      logger: Logger,
      exampleFiles: ExampleFiles,
      platforms: NonEmptyList[model.PlatformId],
      scalas: NonEmptyList[VersionScala],
      name: String,
      bleepVersion: model.Version
  ): model.Build = {
    val defaultOpts = Options(Set(Options.Opt.WithArgs("-encoding", List("utf8")), Options.Opt.Flag("-feature"), Options.Opt.Flag("-unchecked")))

    def variants(name: String): NonEmptyList[(model.PlatformId, VersionScala, model.CrossProjectName)] =
      for {
        p <- platforms
        s <- scalas
      } yield (p, s, model.CrossProjectName(model.ProjectName(name), model.CrossId.defaultFrom(Some(s), Some(p), isFull = false)))

    val mainProjects: NonEmptyList[(model.CrossProjectName, model.Project)] =
      variants(name).map { case (platformId, scala, crossName) =>
        val p = model.Project(
          `extends` = JsonSet.empty,
          cross = JsonMap.empty,
          folder = None,
          dependsOn = JsonSet.empty,
          `source-layout` = Some(SourceLayout.Normal),
          `sbt-scope` = None,
          sources = JsonSet.empty,
          resources = JsonSet.empty,
          dependencies = JsonSet(exampleFiles.fansi),
          java = None,
          scala = Some(model.Scala(version = Some(scala), options = defaultOpts, setup = None, compilerPlugins = JsonSet.empty, strict = Some(true))),
          platform = Some(
            platformId match {
              case model.PlatformId.Jvm =>
                model.Platform.Jvm(Options.empty, jvmMainClass = Some(exampleFiles.main.cls), jvmRuntimeOptions = Options.empty)
              case model.PlatformId.Js =>
                model.Platform.Js(VersionScalaJs.ScalaJs1, None, None, None, None, jsNodeVersion = Some(constants.Node), Some(exampleFiles.main.cls))
              case model.PlatformId.Native =>
                sys.error("native not implemented yet")
            }
          ),
          isTestProject = None,
          testFrameworks = JsonSet.empty
        )
        (crossName, p)
      }

    val testProjects: NonEmptyList[(model.CrossProjectName, model.Project)] =
      variants(s"$name-test").map { case (platformId, scala, crossName) =>
        val p = model.Project(
          `extends` = JsonSet.empty,
          cross = JsonMap.empty,
          folder = None,
          dependsOn = JsonSet(model.ProjectName(name)),
          `source-layout` = Some(SourceLayout.Normal),
          `sbt-scope` = None,
          sources = JsonSet.empty,
          resources = JsonSet.empty,
          dependencies = JsonSet(exampleFiles.scalatest),
          java = None,
          scala = Some(model.Scala(version = Some(scala), options = defaultOpts, setup = None, compilerPlugins = JsonSet.empty, strict = Some(true))),
          platform = Some(
            platformId match {
              case model.PlatformId.Jvm    => model.Platform.Jvm(Options.empty, jvmMainClass = None, jvmRuntimeOptions = Options.empty)
              case model.PlatformId.Js     => model.Platform.Js(VersionScalaJs.ScalaJs1, None, None, None, None, Some(constants.Node), None)
              case model.PlatformId.Native => sys.error("native not implemented yet")
            }
          ),
          isTestProject = Some(true),
          testFrameworks = JsonSet.empty
        )
        (crossName, p)
      }

    val explodedBuild = ExplodedBuild(
      build = model.Build.empty(bleepVersion).copy(jvm = Some(model.Jvm.graalvm)),
      templates = Map.empty,
      projects = (mainProjects.toList ++ testProjects.toList).toMap
    )

    Templates.apply(logger, explodedBuild, ignoreWhenInferringTemplates = _ => false)
  }
}
