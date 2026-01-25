package bleep

import bleep.commands.Fmt
import org.scalactic.TripleEqualsSupport
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Paths

class JavaFmtTest extends AnyFunSuite with TripleEqualsSupport {

  test("parseConfig returns default config for empty input") {
    val config = Fmt.JavaFmt.parseConfig("")
    assert(config === Fmt.JavaFmt.JavaFmtConfig.default)
    assert(config.enabled === true)
  }

  test("parseConfig parses enabled = false") {
    val input = "enabled = false"
    val config = Fmt.JavaFmt.parseConfig(input)
    assert(config.enabled === false)
  }

  test("parseConfig parses all options") {
    val input =
      """enabled = true
        |version = 1.20.0
        |style = aosp
        |skipJavadocFormatting = true
        |excludePaths = ["glob:**/generated/**", "glob:**/build/**"]
        |""".stripMargin
    val config = Fmt.JavaFmt.parseConfig(input)
    assert(config.enabled === true)
    assert(config.version === "1.20.0")
    assert(config.style === "aosp")
    assert(config.skipJavadocFormatting === true)
    assert(config.excludePaths === List("glob:**/generated/**", "glob:**/build/**"))
  }

  test("parseConfig parses multi-line arrays") {
    val input =
      """excludePaths = [
        |  "glob:**/generated-and-checked-in/**",
        |  "glob:**/build/**"
        |]
        |""".stripMargin
    val config = Fmt.JavaFmt.parseConfig(input)
    assert(config.excludePaths === List("glob:**/generated-and-checked-in/**", "glob:**/build/**"))
  }

  test("matchesExcludePattern filters files in generated directory") {
    val buildDir = Paths.get("/project")
    val patterns = List("glob:**/generated/**")

    val generated = Paths.get("/project/src/generated/Foo.java")
    val normal = Paths.get("/project/src/main/Foo.java")

    assert(Fmt.JavaFmt.matchesExcludePattern(generated, buildDir, patterns) === true)
    assert(Fmt.JavaFmt.matchesExcludePattern(normal, buildDir, patterns) === false)
  }

  test("matchesExcludePattern supports multiple patterns") {
    val buildDir = Paths.get("/project")
    val patterns = List("glob:**/generated/**", "glob:**/build/**")

    val inGenerated = Paths.get("/project/src/generated/Foo.java")
    val inBuild = Paths.get("/project/build/Bar.java")
    val inMain = Paths.get("/project/src/main/Baz.java")

    assert(Fmt.JavaFmt.matchesExcludePattern(inGenerated, buildDir, patterns) === true)
    assert(Fmt.JavaFmt.matchesExcludePattern(inBuild, buildDir, patterns) === true)
    assert(Fmt.JavaFmt.matchesExcludePattern(inMain, buildDir, patterns) === false)
  }

  test("matchesExcludePattern works with and without glob: prefix") {
    val buildDir = Paths.get("/project")
    val path = Paths.get("/project/src/generated/Foo.java")

    assert(Fmt.JavaFmt.matchesExcludePattern(path, buildDir, List("glob:**/generated/**")) === true)
    assert(Fmt.JavaFmt.matchesExcludePattern(path, buildDir, List("**/generated/**")) === true)
  }

  test("buildFlags maps config options to CLI flags") {
    val config = Fmt.JavaFmt.JavaFmtConfig.default.copy(
      style = "aosp",
      skipJavadocFormatting = true
    )
    val flags = Fmt.JavaFmt.buildFlags(config)
    assert(flags.contains("--aosp"))
    assert(flags.contains("--skip-javadoc-formatting"))
  }

}
