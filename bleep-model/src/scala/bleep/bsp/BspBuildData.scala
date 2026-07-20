package bleep.bsp

import bleep.model
import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

import java.nio.file.Path

/** Protocol for passing rewritten build to BSP server.
  *
  * When bleep (client) starts a BSP session with bleep-bsp (server), the client can pass an already-rewritten build to the server via the BSP initialize
  * request's data field. This allows the server to use the exact same build state as the client, including any build rewrites like ReplaceBleepDependencies.
  */
object BspBuildData {

  /** The dataKind value used in BSP initialize params */
  val DataKind = "bleep-build"

  /** Payload containing variant name and the exploded build.
    *
    * @param variantName
    *   The build variant name (e.g., "normal", "bsp")
    * @param build
    *   The fully exploded build model
    * @param classpathOverrides
    *   Additional classpath entries per project, used by ReplaceBleepDependencies to add bleep's own class directories to test builds that depend on bleep
    * @param buildId
    *   Content hash of the three fields above. Construct via [[Payload.of]] rather than passing this directly, so it cannot drift from what it identifies.
    */
  case class Payload(
      variantName: String,
      build: model.Build.Exploded,
      classpathOverrides: Map[model.CrossProjectName, List[Path]],
      buildId: BuildId
  )

  object Payload {

    /** Build a payload, deriving its [[BuildId]] from its contents. */
    def of(
        variantName: String,
        build: model.Build.Exploded,
        classpathOverrides: Map[model.CrossProjectName, List[Path]]
    ): Payload =
      Payload(variantName, build, classpathOverrides, buildIdOf(variantName, build, classpathOverrides))

    // Path encoder/decoder
    private implicit val pathEncoder: Encoder[Path] = Encoder[String].contramap(_.toString)
    private implicit val pathDecoder: Decoder[Path] = Decoder[String].map(Path.of(_))

    // Map encoder/decoder for CrossProjectName keys
    private implicit val mapEncoder: Encoder[Map[model.CrossProjectName, List[Path]]] =
      Encoder.encodeMap[model.CrossProjectName, List[Path]](using model.CrossProjectName.keyEncodes, Encoder.encodeList[Path])
    private implicit val mapDecoder: Decoder[Map[model.CrossProjectName, List[Path]]] =
      Decoder.decodeMap[model.CrossProjectName, List[Path]](using model.CrossProjectName.keyDecodes, Decoder.decodeList[Path])

    implicit val encoder: Encoder[Payload] = deriveEncoder
    implicit val decoder: Decoder[Payload] = deriveDecoder

    def encode(p: Payload): String = {
      import io.circe.syntax._
      p.asJson.noSpaces
    }

    /** Hash of everything a payload carries, excluding the id itself. */
    private def buildIdOf(
        variantName: String,
        build: model.Build.Exploded,
        classpathOverrides: Map[model.CrossProjectName, List[Path]]
    ): BuildId = {
      import io.circe.syntax._
      BuildId.ofJson(
        Json.obj(
          "variantName" -> variantName.asJson,
          "build" -> build.asJson,
          "classpathOverrides" -> classpathOverrides.asJson
        )
      )
    }
  }
}
