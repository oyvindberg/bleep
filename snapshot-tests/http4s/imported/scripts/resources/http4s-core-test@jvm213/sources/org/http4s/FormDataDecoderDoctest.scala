package org.http4s

import _root_.munit._

class `FormDataDecoderDoctest` extends FunSuite {

  def sbtDoctestTypeEquals[A](a1: => A)(a2: => A): _root_.scala.Unit = {
    val _ = () => (a1, a2)
  }
  def sbtDoctestReplString(any: _root_.scala.Any): _root_.scala.Predef.String =
    _root_.com.github.tkawachi.doctest.DoctestRuntime.replStringOf(any)

  test("FormDataDecoder.scala:26: FormDataDecoder") {
    import cats.syntax.all._

    import cats.data._

    import org.http4s.FormDataDecoder._

    case class Foo(a: String, b: Boolean)

    case class Bar(fs: List[Foo], f: Foo, d: Boolean)

    implicit val fooMapper: FormDataDecoder[Foo] = (
      field[String]("a"),
      field[Boolean]("b"),
    ).mapN(Foo.apply)

    val barMapper = (
      list[Foo]("fs"),
      nested[Foo]("f"),
      field[Boolean]("d"),
    ).mapN(Bar.apply)

    // example at line 47: barMapper( ...
    sbtDoctestTypeEquals(
      barMapper(
        Map(
          "fs[].a" -> Chain("a1", "a2"),
          "fs[].b" -> Chain("true", "false"),
          "f.a" -> Chain("fa"),
          "f.b" -> Chain("false"),
          "d" -> Chain("true"),
        )
      )
    )(
      barMapper(
        Map(
          "fs[].a" -> Chain("a1", "a2"),
          "fs[].b" -> Chain("true", "false"),
          "f.a" -> Chain("fa"),
          "f.b" -> Chain("false"),
          "d" -> Chain("true"),
        )
      ): ValidatedNel[ParseFailure, Bar]
    )
    assertEquals(
      sbtDoctestReplString(
        barMapper(
          Map(
            "fs[].a" -> Chain("a1", "a2"),
            "fs[].b" -> Chain("true", "false"),
            "f.a" -> Chain("fa"),
            "f.b" -> Chain("false"),
            "d" -> Chain("true"),
          )
        )
      ),
      "Valid(Bar(List(Foo(a1,true), Foo(a2,false)),Foo(fa,false),true))",
    )
  }

}
