package bleep.model

import io.circe.syntax.KeyOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

sealed abstract class CompileServerMode(val mode: String)

object CompileServerMode {
  case object NewEachInvocation extends CompileServerMode("new-each-invocation")
  case object Shared extends CompileServerMode("shared")
  val All = List(NewEachInvocation, Shared)

  implicit val decoder: Decoder[CompileServerMode] =
    Decoder.instance { c =>
      c.downField("mode")
        .as[String]
        .flatMap(str => All.find(_.mode == str).toRight(DecodingFailure(s"$str not among ${All.map(_.mode).mkString(", ")}", c.history)))
    }

  implicit val encoder: Encoder[CompileServerMode] =
    Encoder.instance(config => Json.obj("mode" := config.mode))
}
