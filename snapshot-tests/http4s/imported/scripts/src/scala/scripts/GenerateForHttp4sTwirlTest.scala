
package scripts

import bleep.{BleepCodegenScript, Commands, Started}

import java.nio.file.Files

object GenerateForHttp4sTwirlTest extends BleepCodegenScript("GenerateForHttp4sTwirlTest") {
  override def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit = {
    started.logger.error("This script is a placeholder! You'll need to replace the contents with code which actually generates the files you want")

    targets.foreach { target =>
      if (Set("http4s-twirl-test@jvm213", "http4s-twirl-test@jvm3").contains(target.project.value)) {
        val to = target.sources.resolve("html/test.template.scala")
        started.logger.withContext(target.project).warn(s"Writing $to")
        val content = s"""|
      |package html
      |
      |
      |object test extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template0[play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**/
      |  def apply():play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*1.1*/(${"\"" * 3}<h1>test html</h1>${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(): play.twirl.api.HtmlFormat.Appendable = apply()
      |
      |  def f:(() => play.twirl.api.HtmlFormat.Appendable) = () => apply()
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: twirl/src/test/twirl/test.scala.html
      |                  HASH: f7a2c5333a340176072e80634147b97b3be0cec8
      |                  MATRIX: 400->0
      |                  LINES: 13->1
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set("http4s-twirl-test@jvm213", "http4s-twirl-test@jvm3").contains(target.project.value)) {
        val to = target.sources.resolve("js/test.template.scala")
        started.logger.withContext(target.project).warn(s"Writing $to")
        val content = s"""|
      |package js
      |
      |
      |object test extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.JavaScriptFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.JavaScriptFormat.Appendable]](play.twirl.api.JavaScriptFormat) with _root_.play.twirl.api.Template0[play.twirl.api.JavaScriptFormat.Appendable] {
      |
      |  /**/
      |  def apply():play.twirl.api.JavaScriptFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*1.1*/(${"\"" * 3}"test js${"\"" * 3}"))
      |      }
      |    }
      |  }
      |
      |  def render(): play.twirl.api.JavaScriptFormat.Appendable = apply()
      |
      |  def f:(() => play.twirl.api.JavaScriptFormat.Appendable) = () => apply()
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: twirl/src/test/twirl/test.scala.js
      |                  HASH: b795ec10d4e55c2f3838de49ccc786ca9c41b137
      |                  MATRIX: 428->0
      |                  LINES: 13->1
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set("http4s-twirl-test@jvm213", "http4s-twirl-test@jvm3").contains(target.project.value)) {
        val to = target.sources.resolve("txt/test.template.scala")
        started.logger.withContext(target.project).warn(s"Writing $to")
        val content = s"""|
      |package txt
      |
      |
      |object test extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.TxtFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.TxtFormat.Appendable]](play.twirl.api.TxtFormat) with _root_.play.twirl.api.Template0[play.twirl.api.TxtFormat.Appendable] {
      |
      |  /**/
      |  def apply():play.twirl.api.TxtFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*1.1*/(${"\"" * 3}test text${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(): play.twirl.api.TxtFormat.Appendable = apply()
      |
      |  def f:(() => play.twirl.api.TxtFormat.Appendable) = () => apply()
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: twirl/src/test/twirl/test.scala.txt
      |                  HASH: 6afc05eae22e994f1c7dd48e58f8895dd9028223
      |                  MATRIX: 394->0
      |                  LINES: 13->1
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set("http4s-twirl-test@jvm213", "http4s-twirl-test@jvm3").contains(target.project.value)) {
        val to = target.sources.resolve("xml/test.template.scala")
        started.logger.withContext(target.project).warn(s"Writing $to")
        val content = s"""|
      |package xml
      |
      |
      |object test extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.XmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.XmlFormat.Appendable]](play.twirl.api.XmlFormat) with _root_.play.twirl.api.Template0[play.twirl.api.XmlFormat.Appendable] {
      |
      |  /**/
      |  def apply():play.twirl.api.XmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*1.1*/(${"\"" * 3}<test>test xml</test>${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(): play.twirl.api.XmlFormat.Appendable = apply()
      |
      |  def f:(() => play.twirl.api.XmlFormat.Appendable) = () => apply()
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: twirl/src/test/twirl/test.scala.xml
      |                  HASH: 14cbe0984ade53b19135408f5767afb302a843bb
      |                  MATRIX: 394->0
      |                  LINES: 13->1
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }

  }
}