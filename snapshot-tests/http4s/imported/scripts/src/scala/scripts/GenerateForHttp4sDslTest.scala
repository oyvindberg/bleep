
package scripts

import bleep.{BleepCodegenScript, Commands, Started}

import java.nio.file.Files

object GenerateForHttp4sDslTest extends BleepCodegenScript("GenerateForHttp4sDslTest") {
  override def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit = {
    started.logger.error("This script is a placeholder! You'll need to replace the contents with code which actually generates the files you want")

    targets.foreach { target =>
      if (Set("http4s-dsl-test@jvm213", "http4s-dsl-test@jvm3").contains(target.project.value)) {
        val to = target.sources.resolve("org/http4s/dsl/andDoctest.scala")
        started.logger.withContext(target.project).warn(s"Writing $to")
        val content = s"""|package org.http4s.dsl
      |
      |import _root_.munit._
      |
      |class `andDoctest` extends FunSuite {
      |
      |  def sbtDoctestTypeEquals[A](a1: => A)(a2: => A): _root_.scala.Unit = {
      |    val _ = () => (a1, a2)
      |  }
      |  def sbtDoctestReplString(any: _root_.scala.Any): _root_.scala.Predef.String = {
      |    val s = _root_.scala.runtime.ScalaRunTime.replStringOf(any, 1000).init
      |    if (s.headOption == Some('\\\\n')) s.tail else s
      |  }
      |
      |  test("and.scala:19: &") {
      |    object Even { def unapply(i: Int) = (i % 2) == 0 }
      |
      |    object Positive { def unapply(i: Int) = i > 0 }
      |
      |    def describe(i: Int) = i match {
      |      case org.http4s.dsl.&(Even(), Positive()) => "even and positive"
      |      case Even() => "even but not positive"
      |      case Positive() => "positive but not even"
      |      case _ => "neither even nor positive"
      |    }
      |
      |    //example at line 30: describe(-1)
      |    sbtDoctestTypeEquals(describe(-1))((describe(-1)): String)
      |      assertEquals(sbtDoctestReplString(describe(-1)), "neither even nor positive")
      |
      |    //example at line 32: describe(0)
      |    sbtDoctestTypeEquals(describe(0))((describe(0)): String)
      |      assertEquals(sbtDoctestReplString(describe(0)), "even but not positive")
      |
      |    //example at line 34: describe(1)
      |    sbtDoctestTypeEquals(describe(1))((describe(1)): String)
      |      assertEquals(sbtDoctestReplString(describe(1)), "positive but not even")
      |
      |    //example at line 36: describe(2)
      |    sbtDoctestTypeEquals(describe(2))((describe(2)): String)
      |      assertEquals(sbtDoctestReplString(describe(2)), "even and positive")
      |  }
      |
      |}""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }

  }
}