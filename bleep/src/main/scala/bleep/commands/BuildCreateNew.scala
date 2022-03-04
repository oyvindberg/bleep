package bleep
package commands

import bleep.SourceLayout.Normal
import bleep.internal.FileUtils.DeleteUnknowns
import bleep.internal.{FileUtils, ShortenAndSortJson, Templates}
import bleep.logging.Logger
import bleep.model.{Platform, PlatformId, Scala}
import cats.data.NonEmptyList
import io.circe.syntax._

import java.nio.file.{Files, Path}

case class BuildCreateNew(logger: Logger, cwd: Path, platforms: NonEmptyList[model.PlatformId], scalas: NonEmptyList[Versions.Scala], name: String)
    extends BleepCommand {

  def write(path: Path, content: String, what: String): Unit = {
    FileUtils.writeString(path, content)
    logger.withContext(path).info(s"Creating $what")
  }

  override def run(): Either[BuildException, Unit] =
    generate().map(_ => ())

  def generate(): Either[BuildException, (Started, Map[RelPath, String])] = {

    val exampleFiles = new BuildCreateNew.ExampleFiles(name)
    val build = BuildCreateNew.genBuild(exampleFiles, platforms, scalas, name)
    val buildPaths = BuildPaths.fromBuildDir(cwd, cwd, BuildPaths.Mode.Normal)
    val pre = Prebootstrapped(buildPaths, logger)

    val allFiles = Map(
      RelPath.relativeTo(cwd, buildPaths.bleepJsonFile) -> build.asJson.foldWith(ShortenAndSortJson).spaces2,
      exampleFiles.main.relPath -> exampleFiles.main.contents,
      exampleFiles.test.relPath -> exampleFiles.test.contents
    )

    val syncedFiles = FileUtils.sync(cwd, allFiles, deleteUnknowns = DeleteUnknowns.No, soft = true)

    syncedFiles.foreach { case (path, synced) => logger.info(s"Wrote $path ($synced)") }

    bootstrap.from(pre, rewrites = Nil) map { started =>
      val projects = started.bloopFiles.map { case (_, f) => f.forceGet.project }.toList
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

  def genBuild(exampleFiles: ExampleFiles, platforms: NonEmptyList[model.PlatformId], scalas: NonEmptyList[Versions.Scala], name: String): model.Build = {
    val defaultOpts = Options(Set(Options.Opt.WithArgs("-encoding", List("utf8")), Options.Opt.Flag("-feature"), Options.Opt.Flag("-unchecked")))

    def variants(name: String): NonEmptyList[(PlatformId, Versions.Scala, model.CrossProjectName)] =
      for {
        p <- platforms
        s <- scalas
      } yield (p, s, model.CrossProjectName(model.ProjectName(name), model.CrossId.defaultFrom(Some(s), Some(p))))

    val mainProjects: NonEmptyList[(model.CrossProjectName, model.Project)] =
      variants(name).map { case (platformId, scala, crossName) =>
        val p = model.Project(
          `extends` = JsonList.empty,
          cross = JsonMap.empty,
          folder = None,
          dependsOn = JsonSet.empty,
          `source-layout` = Some(Normal),
          `sbt-scope` = None,
          sources = JsonSet.empty,
          resources = JsonSet.empty,
          dependencies = JsonSet(exampleFiles.fansi),
          java = None,
          scala = Some(Scala(version = Some(scala), options = defaultOpts, setup = None, compilerPlugins = JsonSet.empty)),
          platform = Some(
            platformId match {
              case PlatformId.Jvm    => Platform.Jvm(Options.empty, jvmMainClass = Some(exampleFiles.main.cls), jvmRuntimeOptions = Options.empty)
              case PlatformId.Js     => Platform.Js(Some(Versions.ScalaJs1).filterNot(_ => scala.is3), None, None, None, None, Some(exampleFiles.main.cls))
              case PlatformId.Native => sys.error("native not implemented yet")
            }
          ),
          testFrameworks = JsonSet.empty
        )
        (crossName, p)
      }

    val testProjects: NonEmptyList[(model.CrossProjectName, model.Project)] =
      variants(s"$name-test").map { case (platformId, scala, crossName) =>
        val p = model.Project(
          `extends` = JsonList.empty,
          cross = JsonMap.empty,
          folder = None,
          dependsOn = JsonSet(model.ProjectName(name)),
          `source-layout` = Some(Normal),
          `sbt-scope` = None,
          sources = JsonSet.empty,
          resources = JsonSet.empty,
          dependencies = JsonSet(exampleFiles.scalatest),
          java = None,
          scala = Some(Scala(version = Some(scala), options = defaultOpts, setup = None, compilerPlugins = JsonSet.empty)),
          platform = Some(
            platformId match {
              case PlatformId.Jvm    => Platform.Jvm(Options.empty, jvmMainClass = None, jvmRuntimeOptions = Options.empty)
              case PlatformId.Js     => Platform.Js(Some(Versions.ScalaJs1).filterNot(_ => scala.is3), None, None, None, None, None)
              case PlatformId.Native => sys.error("native not implemented yet")
            }
          ),
          testFrameworks = JsonSet(
            model.TestFrameworkName("com.novocode.junit.JUnitFramework"),
            model.TestFrameworkName("munit.Framework"),
            model.TestFrameworkName("org.scalacheck.ScalaCheckFramework"),
            model.TestFrameworkName("org.scalatest.tools.Framework"),
            model.TestFrameworkName("org.scalatest.tools.ScalaTestFramework"),
            model.TestFrameworkName("org.specs.runner.SpecsFramework"),
            model.TestFrameworkName("org.specs2.runner.Specs2Framework"),
            model.TestFrameworkName("org.specs2.runner.SpecsFramework")
          )
        )
        (crossName, p)
      }

    val explodedBuild = ExplodedBuild(
      templates = Map.empty,
      scripts = Map.empty,
      resolvers = JsonSet.empty,
      projects = (mainProjects.toList ++ testProjects.toList).toMap,
      retainCrossTemplates = Map.empty
    )

    Templates.apply(explodedBuild, ignoreWhenInferringTemplates = _ => false)
  }
}
