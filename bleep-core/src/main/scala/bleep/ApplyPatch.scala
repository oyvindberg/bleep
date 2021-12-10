package bleep

import io.circe.Json
import io.circe.syntax._
import diffson.jsonpatch._

object ApplyPatch {
  type EitherThrow[A] = Either[Throwable, A]
  def apply(patch: JsonPatch[Json], m: model.Build): EitherThrow[model.Build] =
    patch.apply[EitherThrow](m.asJson).flatMap(_.as[model.Build])
}
