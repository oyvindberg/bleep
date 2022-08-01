
package scripts

import java.nio.file.Files

object GenerateResources extends App {
  bleep.bootstrap.forScript("GenerateResources") { (started, commands) =>
    started.logger.error("This script is a placeholder! You'll need to replace the contents with code which actually generates the files you want")

    
    Vector(
      bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-core-test"), Some(bleep.model.CrossId("jvm3"))),
        bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-core-test"), Some(bleep.model.CrossId("jvm213")))).foreach { crossName =>
      val to = started.buildPaths.generatedSourcesDir(crossName).resolve("org/http4s/FormDataDecoderDoctest.scala")
      started.logger.withContext(crossName).warn(s"Writing $to")
      val content = s"""|package org.http4s
      |
      |import _root_.munit._
      |
      |class FormDataDecoderDoctest extends FunSuite {
      |
      |  def sbtDoctestTypeEquals[A](a1: => A)(a2: => A): _root_.scala.Unit = {
      |    val _ = () => (a1, a2)
      |  }
      |  def sbtDoctestReplString(any: _root_.scala.Any): _root_.scala.Predef.String = {
      |    val s = _root_.scala.runtime.ScalaRunTime.replStringOf(any, 1000).init
      |    if (s.headOption == Some('\\n')) s.tail else s
      |  }
      |
      |  test("FormDataDecoder.scala:26: FormDataDecoder") {
      |    import cats.syntax.all._
      |
      |    import cats.data._
      |
      |    import org.http4s.FormDataDecoder._
      |
      |    import org.http4s.ParseFailure
      |
      |    case class Foo(a: String, b: Boolean)
      |
      |    case class Bar(fs: List[Foo], f: Foo, d: Boolean)
      |
      |    implicit val fooMapper: FormDataDecoder[Foo] = (
      |      field[String]("a"),
      |      field[Boolean]("b")
      |    ).mapN(Foo.apply)
      |
      |    val barMapper = (
      |      list[Foo]("fs"),
      |      nested[Foo]("f"),
      |      field[Boolean]("d")
      |    ).mapN(Bar.apply)
      |
      |    //example at line 48: barMapper( ...
      |    sbtDoctestTypeEquals(barMapper(
      |  Map(
      |   "fs[].a" -> Chain("a1", "a2"),
      |   "fs[].b" -> Chain("true", "false"),
      |   "f.a" -> Chain("fa"),
      |   "f.b" -> Chain("false"),
      |   "d" -> Chain("true"))
      |))((barMapper(
      |  Map(
      |   "fs[].a" -> Chain("a1", "a2"),
      |   "fs[].b" -> Chain("true", "false"),
      |   "f.a" -> Chain("fa"),
      |   "f.b" -> Chain("false"),
      |   "d" -> Chain("true"))
      |)): ValidatedNel[ParseFailure, Bar])
      |      assertEquals(sbtDoctestReplString(barMapper(
      |  Map(
      |   "fs[].a" -> Chain("a1", "a2"),
      |   "fs[].b" -> Chain("true", "false"),
      |   "f.a" -> Chain("fa"),
      |   "f.b" -> Chain("false"),
      |   "d" -> Chain("true"))
      |)), "Valid(Bar(List(Foo(a1,true), Foo(a2,false)),Foo(fa,false),true))")
      |  }
      |
      |}""".stripMargin
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }



    Vector(
      bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-core-test"), Some(bleep.model.CrossId("jvm3"))),
        bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-core-test"), Some(bleep.model.CrossId("jvm213")))).foreach { crossName =>
      val to = started.buildPaths.generatedSourcesDir(crossName).resolve("org/http4s/HttpVersionDoctest.scala")
      started.logger.withContext(crossName).warn(s"Writing $to")
      val content = s"""|package org.http4s
      |
      |import _root_.munit._
      |
      |class HttpVersionDoctest extends FunSuite {
      |
      |  def sbtDoctestTypeEquals[A](a1: => A)(a2: => A): _root_.scala.Unit = {
      |    val _ = () => (a1, a2)
      |  }
      |  def sbtDoctestReplString(any: _root_.scala.Any): _root_.scala.Predef.String = {
      |    val s = _root_.scala.runtime.ScalaRunTime.replStringOf(any, 1000).init
      |    if (s.headOption == Some('\\n')) s.tail else s
      |  }
      |
      |  test("HttpVersion.scala:51: render") {
      |    //example at line 54: HttpVersion.`HTTP/1.1`.renderString
      |    
      |      assertEquals(sbtDoctestReplString(HttpVersion.`HTTP/1.1`.renderString), "HTTP/1.1")
      |  }
      |
      |  test("HttpVersion.scala:60: compare") {
      |    //example at line 63: List(HttpVersion.`HTTP/1.0`, HttpVersion.`HTTP/1.1`, HttpVer ...
      |    
      |      assertEquals(sbtDoctestReplString(List(HttpVersion.`HTTP/1.0`, HttpVersion.`HTTP/1.1`, HttpVersion.`HTTP/0.9`).sorted), "List(HTTP/0.9, HTTP/1.0, HTTP/1.1)")
      |  }
      |
      |  test("HttpVersion.scala:159: fromString") {
      |    //example at line 162: HttpVersion.fromString(\\"HTTP/1.1\\")
      |    
      |      assertEquals(sbtDoctestReplString(HttpVersion.fromString("HTTP/1.1")), "Right(HTTP/1.1)")
      |  }
      |
      |  test("HttpVersion.scala:184: fromVersion") {
      |    //example at line 187: HttpVersion.fromVersion(1, 1)
      |    
      |      assertEquals(sbtDoctestReplString(HttpVersion.fromVersion(1, 1)), "Right(HTTP/1.1)")
      |
      |    //example at line 190: HttpVersion.fromVersion(1, 10)
      |    
      |      assertEquals(sbtDoctestReplString(HttpVersion.fromVersion(1, 10)), "Left(org.http4s.ParseFailure: Invalid HTTP version: major must be <= 9: 10)")
      |  }
      |
      |}""".stripMargin
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }



    Vector(
      bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-core-test"), Some(bleep.model.CrossId("jvm3"))),
        bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-core-test"), Some(bleep.model.CrossId("jvm213")))).foreach { crossName =>
      val to = started.buildPaths.generatedSourcesDir(crossName).resolve("org/http4s/MessageDoctest.scala")
      started.logger.withContext(crossName).warn(s"Writing $to")
      val content = s"""|package org.http4s
      |
      |import _root_.munit._
      |
      |class MessageDoctest extends FunSuite {
      |
      |  def sbtDoctestTypeEquals[A](a1: => A)(a2: => A): _root_.scala.Unit = {
      |    val _ = () => (a1, a2)
      |  }
      |  def sbtDoctestReplString(any: _root_.scala.Any): _root_.scala.Predef.String = {
      |    val s = _root_.scala.runtime.ScalaRunTime.replStringOf(any, 1000).init
      |    if (s.headOption == Some('\\n')) s.tail else s
      |  }
      |
      |  test("Message.scala:134: putHeaders") {
      |    import cats.effect.IO
      |
      |    import org.http4s.headers.Accept
      |
      |    val req = Request[IO]().putHeaders(Accept(MediaRange.`application/*`))
      |
      |    //example at line 142: req.headers.get[Accept]
      |    
      |      assertEquals(sbtDoctestReplString(req.headers.get[Accept]), "Some(Accept(NonEmptyList(application/*)))")
      |
      |    val req2 = req.putHeaders(Accept(MediaRange.`text/*`))
      |
      |    //example at line 146: req2.headers.get[Accept]
      |    
      |      assertEquals(sbtDoctestReplString(req2.headers.get[Accept]), "Some(Accept(NonEmptyList(text/*)))")
      |  }
      |
      |  test("Message.scala:154: addHeader") {
      |    import cats.effect.IO
      |
      |    import org.http4s.headers.Accept
      |
      |    val req = Request[IO]().addHeader(Accept(MediaRange.`application/*`))
      |
      |    //example at line 163: req.headers.get[Accept]
      |    
      |      assertEquals(sbtDoctestReplString(req.headers.get[Accept]), "Some(Accept(NonEmptyList(application/*)))")
      |
      |    val req2 = req.addHeader(Accept(MediaRange.`text/*`))
      |
      |    //example at line 167: req2.headers.get[Accept]
      |    
      |      assertEquals(sbtDoctestReplString(req2.headers.get[Accept]), "Some(Accept(NonEmptyList(application/*, text/*)))")
      |  }
      |
      |}""".stripMargin
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }



    Vector(
      bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-core-test"), Some(bleep.model.CrossId("jvm3"))),
        bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-core-test"), Some(bleep.model.CrossId("jvm213")))).foreach { crossName =>
      val to = started.buildPaths.generatedSourcesDir(crossName).resolve("org/http4s/QueryOpsDoctest.scala")
      started.logger.withContext(crossName).warn(s"Writing $to")
      val content = s"""|package org.http4s
      |
      |import _root_.munit._
      |
      |class QueryOpsDoctest extends FunSuite {
      |
      |  def sbtDoctestTypeEquals[A](a1: => A)(a2: => A): _root_.scala.Unit = {
      |    val _ = () => (a1, a2)
      |  }
      |  def sbtDoctestReplString(any: _root_.scala.Any): _root_.scala.Predef.String = {
      |    val s = _root_.scala.runtime.ScalaRunTime.replStringOf(any, 1000).init
      |    if (s.headOption == Some('\\n')) s.tail else s
      |  }
      |
      |  test("QueryOps.scala:56: ++?") {
      |    import org.http4s.implicits._
      |
      |    //example at line 60: uri\\"www.scala.com\\".++?(\\"key\\" -> List(\\"value1\\", \\"value2\\", \\"va ...
      |    sbtDoctestTypeEquals(uri"www.scala.com".++?("key" -> List("value1", "value2", "value3")))((uri"www.scala.com".++?("key" -> List("value1", "value2", "value3"))): Uri)
      |      assertEquals(sbtDoctestReplString(uri"www.scala.com".++?("key" -> List("value1", "value2", "value3"))), "www.scala.com?key=value1&key=value2&key=value3")
      |  }
      |
      |}""".stripMargin
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }



    Vector(
      bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-core"), Some(bleep.model.CrossId("js213"))),
        bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-core"), Some(bleep.model.CrossId("jvm213")))).foreach { crossName =>
      val to = started.buildPaths.generatedSourcesDir(crossName).resolve("sbt-buildinfo/BuildInfo.scala")
      started.logger.withContext(crossName).warn(s"Writing $to")
      val content = s"""|// $$COVERAGE-OFF$$
      |package org.http4s
      |
      |/** This object was generated by sbt-buildinfo. */
      |case object BuildInfo {
      |  /** The value is "1.0-bc06627-SNAPSHOT". */
      |  val version: String = "1.0-bc06627-SNAPSHOT"
      |  /** The value is "2.13.8". */
      |  val scalaVersion: String = "2.13.8"
      |  /** The value is (1 -> 0). */
      |  val apiVersion = (1 -> 0)
      |  override val toString: String = {
      |    "version: %s, scalaVersion: %s, apiVersion: %s".format(
      |      version, scalaVersion, apiVersion
      |    )
      |  }
      |}
      |// $$COVERAGE-ON$$""".stripMargin
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }



    Vector(
      bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-core"), Some(bleep.model.CrossId("jvm3"))),
        bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-core"), Some(bleep.model.CrossId("js3")))).foreach { crossName =>
      val to = started.buildPaths.generatedSourcesDir(crossName).resolve("sbt-buildinfo/BuildInfo.scala")
      started.logger.withContext(crossName).warn(s"Writing $to")
      val content = s"""|// $$COVERAGE-OFF$$
      |package org.http4s
      |
      |/** This object was generated by sbt-buildinfo. */
      |case object BuildInfo {
      |  /** The value is "1.0-bc06627-SNAPSHOT". */
      |  val version: String = "1.0-bc06627-SNAPSHOT"
      |  /** The value is "3.1.1". */
      |  val scalaVersion: String = "3.1.1"
      |  /** The value is (1 -> 0). */
      |  val apiVersion = (1 -> 0)
      |  override val toString: String = {
      |    "version: %s, scalaVersion: %s, apiVersion: %s".format(
      |      version, scalaVersion, apiVersion
      |    )
      |  }
      |}
      |// $$COVERAGE-ON$$""".stripMargin
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }



    Vector(
      bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-dsl-test"), Some(bleep.model.CrossId("jvm3"))),
        bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-dsl-test"), Some(bleep.model.CrossId("jvm213")))).foreach { crossName =>
      val to = started.buildPaths.generatedSourcesDir(crossName).resolve("org/http4s/dsl/andDoctest.scala")
      started.logger.withContext(crossName).warn(s"Writing $to")
      val content = s"""|package org.http4s.dsl
      |
      |import _root_.munit._
      |
      |class andDoctest extends FunSuite {
      |
      |  def sbtDoctestTypeEquals[A](a1: => A)(a2: => A): _root_.scala.Unit = {
      |    val _ = () => (a1, a2)
      |  }
      |  def sbtDoctestReplString(any: _root_.scala.Any): _root_.scala.Predef.String = {
      |    val s = _root_.scala.runtime.ScalaRunTime.replStringOf(any, 1000).init
      |    if (s.headOption == Some('\\n')) s.tail else s
      |  }
      |
      |  test("and.scala:19: &") {
      |    import org.http4s.dsl.&
      |
      |    object Even { def unapply(i: Int) = (i % 2) == 0 }
      |
      |    object Positive { def unapply(i: Int) = i > 0 }
      |
      |    def describe(i: Int) = i match {
      |      case Even() & Positive() => "even and positive"
      |      case Even() => "even but not positive"
      |      case Positive() => "positive but not even"
      |      case _ => "neither even nor positive"
      |    }
      |
      |    //example at line 31: describe(-1)
      |    sbtDoctestTypeEquals(describe(-1))((describe(-1)): String)
      |      assertEquals(sbtDoctestReplString(describe(-1)), "neither even nor positive")
      |
      |    //example at line 33: describe(0)
      |    sbtDoctestTypeEquals(describe(0))((describe(0)): String)
      |      assertEquals(sbtDoctestReplString(describe(0)), "even but not positive")
      |
      |    //example at line 35: describe(1)
      |    sbtDoctestTypeEquals(describe(1))((describe(1)): String)
      |      assertEquals(sbtDoctestReplString(describe(1)), "positive but not even")
      |
      |    //example at line 37: describe(2)
      |    sbtDoctestTypeEquals(describe(2))((describe(2)): String)
      |      assertEquals(sbtDoctestReplString(describe(2)), "even and positive")
      |  }
      |
      |}""".stripMargin
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }



    Vector(
      bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-server-test"), Some(bleep.model.CrossId("js3"))),
        bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-server-test"), Some(bleep.model.CrossId("js213")))).foreach { crossName =>
      val to = started.buildPaths.generatedSourcesDir(crossName).resolve("sbt-buildinfo/BuildInfo.scala")
      started.logger.withContext(crossName).warn(s"Writing $to")
      val content = s"""|// $$COVERAGE-OFF$$
      |package org.http4s.server.test
      |
      |/** This object was generated by sbt-buildinfo. */
      |case object BuildInfo {
      |  /** The value is "/Users/oyvind/bleep/snapshot-tests-in/http4s/server/js/src/test/resources". */
      |  val test_resourceDirectory = "/Users/oyvind/bleep/snapshot-tests-in/http4s/server/js/src/test/resources"
      |  override val toString: String = {
      |    "test_resourceDirectory: %s".format(
      |      test_resourceDirectory
      |    )
      |  }
      |}
      |// $$COVERAGE-ON$$""".stripMargin
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }



    Vector(
      bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-server-test"), Some(bleep.model.CrossId("jvm3"))),
        bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-server-test"), Some(bleep.model.CrossId("jvm213")))).foreach { crossName =>
      val to = started.buildPaths.generatedSourcesDir(crossName).resolve("sbt-buildinfo/BuildInfo.scala")
      started.logger.withContext(crossName).warn(s"Writing $to")
      val content = s"""|// $$COVERAGE-OFF$$
      |package org.http4s.server.test
      |
      |/** This object was generated by sbt-buildinfo. */
      |case object BuildInfo {
      |  /** The value is "/Users/oyvind/bleep/snapshot-tests-in/http4s/server/jvm/src/test/resources". */
      |  val test_resourceDirectory = "/Users/oyvind/bleep/snapshot-tests-in/http4s/server/jvm/src/test/resources"
      |  override val toString: String = {
      |    "test_resourceDirectory: %s".format(
      |      test_resourceDirectory
      |    )
      |  }
      |}
      |// $$COVERAGE-ON$$""".stripMargin
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }



    Vector(
      bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-twirl-test"), Some(bleep.model.CrossId("jvm213"))),
        bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-twirl-test"), Some(bleep.model.CrossId("jvm3")))).foreach { crossName =>
      val to = started.buildPaths.generatedSourcesDir(crossName).resolve("html/test.template.scala")
      started.logger.withContext(crossName).warn(s"Writing $to")
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



    Vector(
      bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-twirl-test"), Some(bleep.model.CrossId("jvm213"))),
        bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-twirl-test"), Some(bleep.model.CrossId("jvm3")))).foreach { crossName =>
      val to = started.buildPaths.generatedSourcesDir(crossName).resolve("js/test.template.scala")
      started.logger.withContext(crossName).warn(s"Writing $to")
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



    Vector(
      bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-twirl-test"), Some(bleep.model.CrossId("jvm213"))),
        bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-twirl-test"), Some(bleep.model.CrossId("jvm3")))).foreach { crossName =>
      val to = started.buildPaths.generatedSourcesDir(crossName).resolve("txt/test.template.scala")
      started.logger.withContext(crossName).warn(s"Writing $to")
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



    Vector(
      bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-twirl-test"), Some(bleep.model.CrossId("jvm213"))),
        bleep.model.CrossProjectName(bleep.model.ProjectName("http4s-twirl-test"), Some(bleep.model.CrossId("jvm3")))).foreach { crossName =>
      val to = started.buildPaths.generatedSourcesDir(crossName).resolve("xml/test.template.scala")
      started.logger.withContext(crossName).warn(s"Writing $to")
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



    Vector(
      bleep.model.CrossProjectName(bleep.model.ProjectName("scalafixinternaltests-test"), Some(bleep.model.CrossId("jvm213")))).foreach { crossName =>
      val to = started.buildPaths.generatedResourcesDir(crossName).resolve("scalafix-testkit.properties")
      started.logger.withContext(crossName).warn(s"Writing $to")
      val content = s"""|#Input data for scalafix testkit
      |#Wed Jul 27 01:47:35 CEST 2022
      |inputClasspath=/Users/oyvind/bleep/snapshot-tests-in/http4s/scalafix-internal/input/target/scala-2.13/classes\\:/Users/oyvind/bleep/snapshot-tests-in/http4s/core/jvm/target/scala-2.13/classes\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.8/scala-library-2.13.8.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/case-insensitive_2.13/1.2.0/case-insensitive_2.13-1.2.0.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/cats-core_2.13/2.7.0/cats-core_2.13-2.7.0.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/cats-effect-std_2.13/3.3.6/cats-effect-std_2.13-3.3.6.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/cats-parse_2.13/0.3.6/cats-parse_2.13-0.3.6.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/http4s/http4s-crypto_2.13/0.2.2/http4s-crypto_2.13-0.2.2.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/co/fs2/fs2-core_2.13/3.2.5/fs2-core_2.13-3.2.5.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/co/fs2/fs2-io_2.13/3.2.5/fs2-io_2.13-3.2.5.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/comcast/ip4s-core_2.13/3.1.2/ip4s-core_2.13-3.1.2.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/literally_2.13/1.0.2/literally_2.13-1.0.2.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/log4s/log4s_2.13/1.10.0/log4s_2.13-1.10.0.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scodec/scodec-bits_2.13/1.1.30/scodec-bits_2.13-1.1.30.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/vault_2.13/3.1.0/vault_2.13-3.1.0.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/slf4j/slf4j-api/1.7.36/slf4j-api-1.7.36.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/cats-kernel_2.13/2.7.0/cats-kernel_2.13-2.7.0.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/simulacrum-scalafix-annotations_2.13/0.5.4/simulacrum-scalafix-annotations_2.13-0.5.4.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/cats-effect-kernel_2.13/3.3.6/cats-effect-kernel_2.13-3.3.6.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/cats-effect_2.13/3.3.5/cats-effect_2.13-3.3.5.jar
      |sourceroot=/Users/oyvind/bleep/snapshot-tests-in/http4s
      |scalacOptions=-deprecation|-encoding|UTF-8|-feature|-unchecked|-Ywarn-numeric-widen|-Xlint\\:deprecation|-Wunused\\:nowarn|-Wdead-code|-Wextra-implicit|-Wnumeric-widen|-Wunused\\:implicits|-Wunused\\:explicits|-Wunused\\:imports|-Wunused\\:locals|-Wunused\\:params|-Wunused\\:patvars|-Wunused\\:privates|-Wvalue-discard|-Ywarn-dead-code|-Ybackend-parallelism|10|-language\\:_|-Xplugin\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/olegpy/better-monadic-for_2.13/0.3.1/better-monadic-for_2.13-0.3.1.jar|-Xplugin\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/kind-projector_2.13.8/0.13.2/kind-projector_2.13.8-0.13.2.jar|-Xplugin\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scalameta/semanticdb-scalac_2.13.8/4.4.32/semanticdb-scalac_2.13.8-4.4.32.jar|-P\\:semanticdb\\:synthetics\\:on|-Yrangepos|-P\\:semanticdb\\:targetroot\\:/Users/oyvind/bleep/snapshot-tests-in/http4s/scalafix-internal/input/target/scala-2.13/meta
      |outputSourceDirectories=/Users/oyvind/bleep/snapshot-tests-in/http4s/scalafix-internal/output/src/main/scala
      |inputSourceDirectories=/Users/oyvind/bleep/snapshot-tests-in/http4s/scalafix-internal/input/src/main/scala
      |scalaVersion=2.13.8""".stripMargin
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }



    Vector(
      bleep.model.CrossProjectName(bleep.model.ProjectName("scalafixinternaltests-test"), Some(bleep.model.CrossId("jvm3")))).foreach { crossName =>
      val to = started.buildPaths.generatedResourcesDir(crossName).resolve("scalafix-testkit.properties")
      started.logger.withContext(crossName).warn(s"Writing $to")
      val content = s"""|#Input data for scalafix testkit
      |#Wed Jul 27 01:48:25 CEST 2022
      |inputClasspath=/Users/oyvind/bleep/snapshot-tests-in/http4s/scalafix-internal/input/target/scala-3.1.1/classes\\:/Users/oyvind/bleep/snapshot-tests-in/http4s/core/jvm/target/scala-3.1.1/classes\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.1.1/scala3-library_3-3.1.1.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/case-insensitive_3/1.2.0/case-insensitive_3-1.2.0.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/cats-core_3/2.7.0/cats-core_3-2.7.0.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/cats-effect-std_3/3.3.6/cats-effect-std_3-3.3.6.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/cats-parse_3/0.3.6/cats-parse_3-0.3.6.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/http4s/http4s-crypto_3/0.2.2/http4s-crypto_3-0.2.2.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/co/fs2/fs2-core_3/3.2.5/fs2-core_3-3.2.5.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/co/fs2/fs2-io_3/3.2.5/fs2-io_3-3.2.5.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/comcast/ip4s-core_3/3.1.2/ip4s-core_3-3.1.2.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/literally_3/1.0.2/literally_3-1.0.2.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/log4s/log4s_3/1.10.0/log4s_3-1.10.0.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scodec/scodec-bits_3/1.1.30/scodec-bits_3-1.1.30.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/vault_3/3.1.0/vault_3-3.1.0.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.6/scala-library-2.13.6.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/cats-kernel_3/2.7.0/cats-kernel_3-2.7.0.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/simulacrum-scalafix-annotations_3/0.5.4/simulacrum-scalafix-annotations_3-0.5.4.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/cats-effect-kernel_3/3.3.6/cats-effect-kernel_3-3.3.6.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/cats-effect_3/3.3.5/cats-effect_3-3.3.5.jar\\:/Users/oyvind/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/slf4j/slf4j-api/1.7.30/slf4j-api-1.7.30.jar
      |sourceroot=/Users/oyvind/bleep/snapshot-tests-in/http4s
      |scalacOptions=-deprecation|-encoding|UTF-8|-feature|-unchecked|-language\\:implicitConversions|-Ykind-projector|-source\\:3.0-migration|-Xsemanticdb|-semanticdb-target|/Users/oyvind/bleep/snapshot-tests-in/http4s/scalafix-internal/input/target/scala-3.1.1/meta
      |outputSourceDirectories=/Users/oyvind/bleep/snapshot-tests-in/http4s/scalafix-internal/output/src/main/scala
      |inputSourceDirectories=/Users/oyvind/bleep/snapshot-tests-in/http4s/scalafix-internal/input/src/main/scala
      |scalaVersion=3.1.1""".stripMargin
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }

  }
}