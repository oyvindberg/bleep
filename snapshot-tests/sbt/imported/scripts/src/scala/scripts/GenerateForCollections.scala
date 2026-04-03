
package scripts

import bleep.{BleepCodegenScript, Commands, Started}

import java.nio.file.Files

object GenerateForCollections extends BleepCodegenScript("GenerateForCollections") {
  override def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit = {
    started.logger.error("This script is a placeholder! You'll need to replace the contents with code which actually generates the files you want")

    targets.foreach { target =>
      if (Set(s"""|collections@jvm212""".stripMargin, s"""|collections@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|sbt/internal/util/ScalaKeywords.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package sbt.internal.util
      |object ScalaKeywords {
      |  val values = Set("yield", "private", "null", "while", "<:", "object", "abstract", "<%", "catch", "#", ":", "type", "final", "then", "=>", "override", "_", "import", "macro", "var", "protected", "sealed", "def", "val", "super", "new", "trait", "package", "lazy", "if", ".", "forSome", ">:", "false", "=", "this", "@", "implicit", "throw", "true", "match", "class", "return", "<-", "try", "finally", "case", "with", "extends", "for", "do", "else")
      |}""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }

  }
}