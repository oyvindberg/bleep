package bleep.scripts

import bleep.RelPath

import java.nio.file.Path

abstract class Demo(val name: String, val rows: Int = 40, val columns: Int = 100) {
  def script(bleep: Path): String
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

  val all = List(
    runNativeNew,
    runCrossNativeNew,
    importNew
  )
}
