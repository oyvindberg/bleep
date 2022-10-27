package bleep.scripts

import java.nio.file.Path

abstract class Demo(val name: String, val rows: Int = 40, val columns: Int = 100) {
  def script(bleep: Path): String
}

object Demo {
  val all = List(
    new Demo("run-native") {
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
          |# show build file
          |bat bleep.yaml
          |
          |# show generated main file
          |bat mycli/src/scala/com/foo/App.scala
          |
          |# run
          |$bleep run mycli
          |""".stripMargin
    },
    new Demo("run-cross-native-jvm") {
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
    },
    new Demo("import") {
      override def script(bleep: Path): String =
        s"""
           |# git clone an existing build
           |git clone https://github.com/scalameta/munit.git
           |
           |cd munit
           |
           |# import into bleep. note that this is a one-time, slow step
           |$bleep import
           |
           |# list projects
           |$bleep projects
           |
           |# show build file
           |bat --line-range :$rows bleep.yaml
           |
           |# generate resources
           |$bleep generate-resources
           |
           |# run scala 3 jvm tests for all modules
           |$bleep test jvm3
           |""".stripMargin
    }
  )

}
