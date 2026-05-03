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
    language: BuildCreateNew.Language,
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

    bootstrap.from(pre, ResolveProjects.InMemory, rewrites = Nil, config, coursierResolver) map { started =>
      val projects = started.resolvedProjects.values.toList.map(_.forceGet)
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
    val recipe = BuildCreateNew.Recipe.forLanguage(language, name, scalas, platforms)
    val build = BuildCreateNew.genBuild(logger, recipe, bleepVersion)

    Map[Path, String](buildPaths.bleepYamlFile -> yaml.encodeShortened(build)) ++
      recipe.files.map { case (relPath, content) => buildPaths.buildDir / relPath -> content }
  }
}

object BuildCreateNew {

  /** Which language flavor `bleep new` produces. */
  sealed abstract class Language(val value: String)
  object Language {
    case object Java extends Language("java")
    case object Kotlin extends Language("kotlin")
    case object Scala extends Language("scala")

    val All: List[Language] = List(Java, Kotlin, Scala)
    val byName: Map[String, Language] = All.map(l => l.value -> l).toMap
  }

  private val mainPkg = "com.example"

  /** A minimal main + test pair for a given language. The main code does the same thing across langs (`println("Hello, World!")`); the test framework varies.
    */
  case class Recipe(
      mainProjectName: String,
      testProjectName: String,
      mainProject: model.Project,
      testProject: model.Project,
      files: Map[RelPath, String]
  )

  object Recipe {
    def forLanguage(
        language: Language,
        name: String,
        scalas: NonEmptyList[model.VersionScala],
        platforms: NonEmptyList[model.PlatformId]
    ): Recipe = language match {
      case Language.Java   => javaRecipe(name)
      case Language.Kotlin => kotlinRecipe(name)
      case Language.Scala  => scalaRecipe(name, scalas, platforms)
    }

    private def jvmPlatform(mainClass: Option[String]): model.Platform =
      model.Platform.Jvm(model.Options.empty, jvmMainClass = mainClass, jvmRuntimeOptions = model.Options.empty)

    private def emptyProject: model.Project =
      model.Project(
        `extends` = model.JsonSet.empty,
        cross = model.JsonMap.empty,
        folder = None,
        dependsOn = model.JsonSet.empty,
        `source-layout` = None,
        `sbt-scope` = None,
        sources = model.JsonSet.empty,
        resources = model.JsonSet.empty,
        dependencies = model.JsonSet.empty,
        jars = model.JsonSet.empty,
        java = None,
        scala = None,
        kotlin = None,
        platform = None,
        isTestProject = None,
        testFrameworks = model.JsonSet.empty,
        sourcegen = model.JsonSet.empty,
        libraryVersionSchemes = model.JsonSet.empty,
        ignoreEvictionErrors = None,
        publish = None
      )

    private def javaRecipe(name: String): Recipe = {
      val mainClass = s"$mainPkg.Main"
      val mainProject = emptyProject.copy(platform = Some(jvmPlatform(Some(mainClass))))
      val testProject = emptyProject.copy(
        dependsOn = model.JsonSet(model.ProjectName(name)),
        isTestProject = Some(true),
        platform = Some(jvmPlatform(None)),
        dependencies = model.JsonSet(model.Dep.Java("org.junit.jupiter", "junit-jupiter", "5.10.1"))
      )
      val mainSrc =
        s"""package $mainPkg;
           |
           |public class Main {
           |  public static String greet(String name) {
           |    return "Hello, " + name + "!";
           |  }
           |
           |  public static void main(String[] args) {
           |    System.out.println(greet("World"));
           |  }
           |}
           |""".stripMargin
      val testSrc =
        s"""package $mainPkg;
           |
           |import static org.junit.jupiter.api.Assertions.assertEquals;
           |
           |import org.junit.jupiter.api.Test;
           |
           |class MainTest {
           |  @Test
           |  void greetsByName() {
           |    assertEquals("Hello, World!", Main.greet("World"));
           |  }
           |}
           |""".stripMargin
      Recipe(
        mainProjectName = name,
        testProjectName = s"$name-test",
        mainProject = mainProject,
        testProject = testProject,
        files = Map(
          RelPath.force(s"$name/src/java/${mainPkg.replace('.', '/')}/Main.java") -> mainSrc,
          RelPath.force(s"$name-test/src/java/${mainPkg.replace('.', '/')}/MainTest.java") -> testSrc
        )
      )
    }

    private def kotlinRecipe(name: String): Recipe = {
      val mainClass = s"$mainPkg.MainKt"
      val kotlin = model.Kotlin(
        version = Some(model.VersionKotlin.Kotlin23),
        options = model.Options.empty,
        jvmTarget = Some(model.Jvm.graalvm.majorVersion),
        compilerPlugins = model.JsonSet.empty,
        js = None,
        native = None
      )
      val mainProject = emptyProject.copy(
        kotlin = Some(kotlin),
        platform = Some(jvmPlatform(Some(mainClass)))
      )
      val testProject = emptyProject.copy(
        dependsOn = model.JsonSet(model.ProjectName(name)),
        isTestProject = Some(true),
        kotlin = Some(kotlin),
        platform = Some(jvmPlatform(None)),
        dependencies = model.JsonSet(
          model.Dep.Java("io.kotest", "kotest-runner-junit5-jvm", "5.8.0"),
          model.Dep.Java("org.junit.jupiter", "junit-jupiter", "5.10.1")
        )
      )
      val mainSrc =
        s"""package $mainPkg
           |
           |object Main {
           |  fun greet(name: String): String = "Hello, $$name!"
           |}
           |
           |fun main() {
           |  println(Main.greet("World"))
           |}
           |""".stripMargin
      val testSrc =
        s"""package $mainPkg
           |
           |import io.kotest.core.spec.style.FunSpec
           |import io.kotest.matchers.shouldBe
           |
           |class MainTest : FunSpec({
           |  test("greets by name") {
           |    Main.greet("World") shouldBe "Hello, World!"
           |  }
           |})
           |""".stripMargin
      Recipe(
        mainProjectName = name,
        testProjectName = s"$name-test",
        mainProject = mainProject,
        testProject = testProject,
        files = Map(
          RelPath.force(s"$name/src/kotlin/${mainPkg.replace('.', '/')}/Main.kt") -> mainSrc,
          RelPath.force(s"$name-test/src/kotlin/${mainPkg.replace('.', '/')}/MainTest.kt") -> testSrc
        )
      )
    }

    private def scalaRecipe(
        name: String,
        scalas: NonEmptyList[model.VersionScala],
        platforms: NonEmptyList[model.PlatformId]
    ): Recipe = {
      val defaultOpts =
        model.Options(Set(model.Options.Opt.WithArgs("-encoding", List("utf8")), model.Options.Opt.Flag("-feature"), model.Options.Opt.Flag("-unchecked")))

      // For Scala we keep the existing cross-build flexibility: pick the first variant for snippets/sources but model all of them in the build.
      val isCross = scalas.size > 1 || platforms.size > 1
      val sourceLayout = if (isCross) model.SourceLayout.CrossFull else model.SourceLayout.Normal
      val sharedDir = if (isCross) "shared/" else ""
      val mainScala = scalas.head
      val mainPlatform = platforms.head
      val mainClass = s"$mainPkg.helloMain"

      val mainProj = model.Project(
        `extends` = model.JsonSet.empty,
        cross = model.JsonMap.empty,
        folder = None,
        dependsOn = model.JsonSet.empty,
        `source-layout` = Some(sourceLayout),
        `sbt-scope` = None,
        sources = model.JsonSet.empty,
        resources = model.JsonSet.empty,
        dependencies = model.JsonSet.empty,
        jars = model.JsonSet.empty,
        java = None,
        scala = Some(model.Scala(version = Some(mainScala), options = defaultOpts, setup = None, compilerPlugins = model.JsonSet.empty, strict = Some(true))),
        kotlin = None,
        platform = Some(platformFor(mainPlatform, Some(mainClass))),
        isTestProject = None,
        testFrameworks = model.JsonSet.empty,
        sourcegen = model.JsonSet.empty,
        libraryVersionSchemes = model.JsonSet.empty,
        ignoreEvictionErrors = None,
        publish = None
      )

      val testProj = mainProj.copy(
        dependsOn = model.JsonSet(model.ProjectName(name)),
        isTestProject = Some(true),
        dependencies = model.JsonSet(model.Dep.Scala("org.scalameta", "munit", "1.0.0")),
        platform = Some(platformFor(mainPlatform, None))
      )

      val mainSrc =
        s"""package $mainPkg
           |
           |object Main {
           |  def greet(name: String): String = s"Hello, $$name!"
           |}
           |
           |@main def helloMain(): Unit =
           |  println(Main.greet("World"))
           |""".stripMargin
      val testSrc =
        s"""package $mainPkg
           |
           |class MainTest extends munit.FunSuite {
           |  test("greets by name") {
           |    assertEquals(Main.greet("World"), "Hello, World!")
           |  }
           |}
           |""".stripMargin

      Recipe(
        mainProjectName = name,
        testProjectName = s"$name-test",
        mainProject = mainProj,
        testProject = testProj,
        files = Map(
          RelPath.force(s"$name/${sharedDir}src/scala/${mainPkg.replace('.', '/')}/Main.scala") -> mainSrc,
          RelPath.force(s"$name-test/${sharedDir}src/scala/${mainPkg.replace('.', '/')}/MainTest.scala") -> testSrc
        )
      )
    }

    private def platformFor(p: model.PlatformId, mainClass: Option[String]): model.Platform = p match {
      case model.PlatformId.Jvm => model.Platform.Jvm(model.Options.empty, jvmMainClass = mainClass, jvmRuntimeOptions = model.Options.empty)
      case model.PlatformId.Js  =>
        model.Platform.Js(model.VersionScalaJs.ScalaJs1, None, None, None, None, jsNodeVersion = Some(constants.Node), mainClass)
      case model.PlatformId.Native =>
        model.Platform.Native(model.VersionScalaNative.ScalaNative05, Some("immix"), mainClass, None, None, None, None, None, None, None)
    }
  }

  def genBuild(logger: Logger, recipe: Recipe, bleepVersion: model.BleepVersion): model.BuildFile = {
    val explodedBuild = model.Build.Exploded(
      bleepVersion.latestRelease,
      explodedProjects = Map(
        model.CrossProjectName(model.ProjectName(recipe.mainProjectName), None) -> recipe.mainProject,
        model.CrossProjectName(model.ProjectName(recipe.testProjectName), None) -> recipe.testProject
      ),
      resolvers = model.JsonList.empty,
      jvm = Some(model.Jvm.graalvm),
      scripts = Map.empty
    )

    templatesInfer(new BleepTemplateLogger(logger), explodedBuild, ignoreWhenInferringTemplates = _ => false)
  }
}
