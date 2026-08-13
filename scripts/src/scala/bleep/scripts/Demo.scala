package bleep.scripts

import bleep.{DiscardOps, RelPath}

import java.nio.file.Path

abstract class Demo(val name: String, val rows: Int = 40, val columns: Int = 100) {

  /** the script which is recorded. lines starting with `#` render as comments, other lines are typed out and executed */
  def script(bleep: Path): String

  /** files written into the workspace before anything runs. `bleepVersion` is the version reported by the recorded binary */
  def files(bleepVersion: String): Map[RelPath, String] = {
    bleepVersion.discard()
    Map.empty
  }

  /** commands run in the workspace *before* recording starts, so the camera opens on an already-warm workspace */
  def prepareScript(bleep: Path): Option[String] = None

  val expectedYaml: Option[RelPath] = Some(RelPath.force("bleep.yaml"))
}

object Demo {

  object runNativeNew extends Demo("run-native") {
    override def script(bleep: Path): String =
      s"""
         |# create build
         |$bleep build new -p native mycli
         |
         |# show files
         |find . -type f
         |
         |# list projects
         |$bleep projects
         |
         |# show generated main file
         |bat mycli/src/scala/com/foo/App.scala
         |
         |# link
         |$bleep link mycli
         |
         |# run
         |$bleep run mycli
         |""".stripMargin
  }

  object runCrossNativeNew extends Demo("run-cross-native-jvm") {
    override def script(bleep: Path): String =
      s"""
         |# create build
         |$bleep build new --platform native --platform jvm --scala 2.13 --scala 3 mycli
         |
         |# show files
         |find . -type f
         |
         |# list projects
         |$bleep projects
         |
         |# show build file (part one)
         |bat --line-range :$rows bleep.yaml
         |# show build file (part two)
         |bat --line-range $rows: bleep.yaml
         |
         |# show generated main file
         |bat mycli/shared/src/scala/com/foo/App.scala
         |
         |# run
         |$bleep run mycli@native213
         |$bleep run mycli@native3
         |$bleep run mycli@jvm213
         |$bleep run mycli@jvm3
         |""".stripMargin
  }

  object importNew extends Demo("import") {
    override val expectedYaml: Option[RelPath] =
      Some(RelPath.force("./zio-http/bleep.yaml"))

    override def script(bleep: Path): String =
      s"""
         |# git clone an existing build
         |git clone https://github.com/zio/zio-http.git
         |
         |cd zio-http
         |
         |git checkout v2.0.0-RC11>/dev/null
         |
         |# import into bleep. note that this is a one-time, slow step
         |$bleep import
         |
         |# list projects
         |$bleep projects
         |
         |# run tests for one scala 3 module
         |$bleep test zhttp-logging-test@jvm3
         |""".stripMargin
  }

  /** The `bleep test --diff` story: break a function, let the diff pinpoint the newly failing test, fix it, watch the diff report `1 fixed` and then
    * `identical`, and close with a timing diff which suppresses insignificant jitter.
    *
    * All setup (workspace, git init, warm compile server, history entries #1-#3) happens off-camera in [[prepareScript]]; the three on-camera
    * `bleep test --diff` runs then record history entries #4-#6, which is why the closing command can hardcode `history diff 5 6`.
    */
  object diff extends Demo("diff", rows = 30, columns = 100) {
    override val expectedYaml: Option[RelPath] = None

    private val pricingScala =
      """package com.example
        |
        |object Pricing:
        |  /** 10% off for orders of 100 or more */
        |  def discount(orderTotal: Int): Int =
        |    if orderTotal >= 100 then orderTotal * 10 / 100
        |    else 0
        |
        |  def total(orderTotal: Int): Int =
        |    orderTotal - discount(orderTotal)
        |
        |@main def helloMain(): Unit =
        |  println(s"total(240) = ${Pricing.total(240)}")
        |""".stripMargin

    private val pricingTestScala =
      """package com.example
        |
        |class PricingTest extends munit.FunSuite:
        |  test("no discount below 100") {
        |    assertEquals(Pricing.total(80), 80)
        |  }
        |
        |  test("10 percent off at 100 and above") {
        |    assertEquals(Pricing.total(240), 216)
        |  }
        |""".stripMargin

    // `$version` must match the recorded binary exactly: a mismatch either relaunches the wanted version or logs a
    // "Not launching ..." notice into the recording, and `$version: dev` only works inside bleep's own build
    private def bleepYaml(bleepVersion: String) =
      s"""$$schema: https://raw.githubusercontent.com/oyvindberg/bleep/master/schema.json
        |$$version: $bleepVersion
        |jvm:
        |  name: graalvm-community:25.0.1
        |projects:
        |  mathy:
        |    extends: template-common
        |    platform:
        |      mainClass: com.example.helloMain
        |  mathy-test:
        |    dependencies: org.scalameta::munit:1.3.4
        |    dependsOn: mathy
        |    extends: template-common
        |    isTestProject: true
        |templates:
        |  template-common:
        |    platform:
        |      name: jvm
        |    scala:
        |      version: 3.8.4
        |""".stripMargin

    override def files(bleepVersion: String): Map[RelPath, String] =
      Map(
        RelPath.force("bleep.yaml") -> bleepYaml(bleepVersion),
        RelPath.force("mathy/src/scala/com/example/Pricing.scala") -> pricingScala,
        RelPath.force("mathy-test/src/scala/com/example/PricingTest.scala") -> pricingTestScala
      )

    override def prepareScript(bleep: Path): Option[String] =
      Some(
        s"""set -euo pipefail
           |git init -q
           |git add -A
           |git -c user.email=demo@bleep.build -c user.name=demo commit -qm 'initial import'
           |$bleep compile --no-tui --no-color
           |$bleep test --no-tui --no-color
           |$bleep test --no-tui --no-color
           |""".stripMargin
      )

    override def script(bleep: Path): String =
      s"""
         |# every compile and test run in this workspace is recorded
         |$bleep history
         |
         |# introduce a bug: bump the discount from 10% to 15%
         |sed -i '' 's|10 / 100|15 / 100|' mathy/src/scala/com/example/Pricing.scala
         |
         |git diff mathy/src/scala/com/example/Pricing.scala
         |
         |# don't eyeball the log - ask bleep exactly what changed
         |$bleep test --diff
         |
         |# fix the bug
         |sed -i '' 's|15 / 100|10 / 100|' mathy/src/scala/com/example/Pricing.scala
         |
         |# the diff confirms it: one test fixed
         |$bleep test --diff
         |
         |# run once more - no logical differences, nothing to re-review
         |$bleep test --diff
         |
         |# compare any two runs after the fact - timing jitter is suppressed
         |$bleep history diff 5 6 --timing
         |""".stripMargin
  }

  val all = List(
    runNativeNew,
    runCrossNativeNew,
    importNew,
    diff
  )
}
