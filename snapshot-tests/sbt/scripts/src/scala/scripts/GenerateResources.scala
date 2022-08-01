
package scripts

import java.nio.file.Files

object GenerateResources extends App {
  bleep.bootstrap.forScript("GenerateResources") { (started, commands) =>
    started.logger.error("This script is a placeholder! You'll need to replace the contents with code which actually generates the files you want")

    
    Vector(
      bleep.model.CrossProjectName(bleep.model.ProjectName("collections"), Some(bleep.model.CrossId("jvm213"))),
        bleep.model.CrossProjectName(bleep.model.ProjectName("collections"), Some(bleep.model.CrossId("jvm212")))).foreach { crossName =>
      val to = started.buildPaths.generatedSourcesDir(crossName).resolve("sbt/internal/util/ScalaKeywords.scala")
      started.logger.withContext(crossName).warn(s"Writing $to")
      val content = s"""|package sbt.internal.util
      |object ScalaKeywords {
      |  val values = Set("yield", "private", "null", "while", "<:", "object", "abstract", "<%", "catch", "#", ":", "type", "final", "then", "=>", "override", "_", "import", "macro", "var", "protected", "sealed", "def", "val", "super", "new", "trait", "package", "lazy", "if", ".", "forSome", ">:", "false", "=", "this", "@", "implicit", "throw", "true", "match", "class", "return", "<-", "try", "finally", "case", "with", "extends", "for", "do", "else")
      |}""".stripMargin
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }



    Vector(
      bleep.model.CrossProjectName(bleep.model.ProjectName("sbt-dependency-tree"), None)).foreach { crossName =>
      val to = started.buildPaths.generatedResourcesDir(crossName).resolve("sbt/sbt.autoplugins")
      started.logger.withContext(crossName).warn(s"Writing $to")
      val content = s"""|sbt.plugins.DependencyTreePlugin""".stripMargin
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }

  }
}