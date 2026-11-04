package bleep

import bleep.internal.{CheckMisplacedSources, FileUtils}
import org.scalatest.funsuite.AnyFunSuite
import ryddig.LogLevel

import java.nio.file.{Files, Path, Paths}

class CheckMisplacedSourcesTest extends AnyFunSuite {

  private val prelude =
    """$schema: https://raw.githubusercontent.com/oyvindberg/bleep/master/schema.json
      |$version: dev
      |""".stripMargin

  private def runCheck(yaml: String, files: List[String]): List[String] = {
    val tempDir = Files.createTempDirectory("bleep-misplaced-")
    try {
      Files.writeString(tempDir.resolve("bleep.yaml"), prelude ++ yaml)
      files.foreach { rel =>
        val target = tempDir.resolve(rel)
        Files.createDirectories(target.getParent)
        Files.writeString(target, "")
      }

      val existing = BuildLoader.find(tempDir).existing.orThrow
      val build = model.Build.FileBacked(existing.buildFile.forceGet.orThrow)
      val buildPaths = BuildPaths(cwd = tempDir, existing, model.BuildVariant.Normal)

      val logger = ThreadSafeStoringLogger().withMinLogLevel(LogLevel.debug)
      CheckMisplacedSources(logger, buildPaths, build)
      logger.underlying.toList.filter(_.metadata.logLevel == LogLevel.warn).map(_.message.plainText)
    } finally FileUtils.deleteDirectory(tempDir)
  }

  private val kotlinYaml =
    """projects:
      |  myapp:
      |    source-layout: kotlin
      |    kotlin:
      |      version: 2.1.20
      |    platform:
      |      name: jvm
      |""".stripMargin

  test("kotlin source under src/main/kotlin warns") {
    val warns = runCheck(kotlinYaml, List("myapp/src/main/kotlin/test/Foo.kt"))
    assert(warns.size === 1, warns)
    assert(warns.head.contains("kotlin"))
    assert(warns.head.contains("src/main/kotlin"))
  }

  test("kotlin source at the correct src/kotlin does not warn") {
    val warns = runCheck(kotlinYaml, List("myapp/src/kotlin/test/Foo.kt"))
    assert(warns === Nil)
  }

  test("empty src/main/kotlin directory does not warn") {
    val tempDir = Files.createTempDirectory("bleep-misplaced-empty-")
    try {
      Files.createDirectories(tempDir.resolve("myapp/src/main/kotlin/test"))
      Files.writeString(tempDir.resolve("bleep.yaml"), prelude ++ kotlinYaml)
      val existing = BuildLoader.find(tempDir).existing.orThrow
      val build = model.Build.FileBacked(existing.buildFile.forceGet.orThrow)
      val buildPaths = BuildPaths(cwd = tempDir, existing, model.BuildVariant.Normal)
      val logger = ThreadSafeStoringLogger().withMinLogLevel(LogLevel.debug)
      CheckMisplacedSources(logger, buildPaths, build)
      val warns = logger.underlying.toList.filter(_.metadata.logLevel == LogLevel.warn)
      assert(warns === Nil)
    } finally FileUtils.deleteDirectory(tempDir)
  }

  test("project with explicit sources: override does not warn") {
    val yaml =
      """projects:
        |  myapp:
        |    source-layout: kotlin
        |    kotlin:
        |      version: 2.1.20
        |    platform:
        |      name: jvm
        |    sources: ./src/main/kotlin
        |""".stripMargin
    val warns = runCheck(yaml, List("myapp/src/main/kotlin/test/Foo.kt"))
    assert(warns === Nil)
  }

  test("project with sbt-scope: main does not warn") {
    val yaml =
      """projects:
        |  myapp:
        |    source-layout: kotlin
        |    sbt-scope: main
        |    kotlin:
        |      version: 2.1.20
        |    platform:
        |      name: jvm
        |""".stripMargin
    val warns = runCheck(yaml, List("myapp/src/main/kotlin/test/Foo.kt"))
    assert(warns === Nil)
  }

  test("java source under src/main/java warns") {
    val yaml =
      """projects:
        |  myapp:
        |    java: {}
        |    platform:
        |      name: jvm
        |""".stripMargin
    val warns = runCheck(yaml, List("myapp/src/main/java/test/Foo.java"))
    assert(warns.size === 1, warns)
    assert(warns.head.contains("java"))
    assert(warns.head.contains("src/main/java"))
  }

  test("scala source under src/main/scala warns") {
    val yaml =
      """projects:
        |  myapp:
        |    scala:
        |      version: 3.3.3
        |    platform:
        |      name: jvm
        |""".stripMargin
    val warns = runCheck(yaml, List("myapp/src/main/scala/test/Foo.scala"))
    assert(warns.size === 1, warns)
    assert(warns.head.contains("scala"))
    assert(warns.head.contains("src/main/scala"))
  }

  test("src/test/<lang> is also caught") {
    val warns = runCheck(kotlinYaml, List("myapp/src/test/kotlin/test/Foo.kt"))
    assert(warns.size === 1, warns)
    assert(warns.head.contains("src/test/kotlin"))
  }
}
