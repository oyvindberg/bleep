package bleep.model

import bleep.internal.EnumCodec
import io.circe.Codec

sealed abstract class CompileOrder(val id: String)

object CompileOrder {
  case object JavaThenScala extends CompileOrder("java->scala")

  case object ScalaThenJava extends CompileOrder("scala->java")

  /** Scalac sees the `.java` sources too (for their signatures) before javac compiles them. The only order that supports Java and Scala in one project
    * referring to each other — `java->scala` and `scala->java` each require one side to compile against nothing from the other.
    */
  case object Mixed extends CompileOrder("mixed")

  final val All: Seq[CompileOrder] = List(JavaThenScala, ScalaThenJava, Mixed)

  implicit val codec: Codec[CompileOrder] =
    EnumCodec.codec(All.map(x => x.id -> x).toMap)
}
