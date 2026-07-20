package bleep.bsp

import bleep.{model, ResolvedProject, Started}
import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

/** Protocol for passing a resolved build from bleep (the client) to bleep-bsp (the server).
  *
  * The client loads the build, resolves every project, and ships the result in the BSP initialize request's data field. The server then compiles what it was
  * given rather than loading and resolving a build of its own, so the two cannot disagree about what is being built.
  *
  * Paths are absolute and meaningful only on the machine that produced them. That is fine because the daemon is forked by the client, on the same machine,
  * sharing its coursier cache and workspace. Shipping a build to another machine is out of scope.
  */
object BspBuildData {

  /** The dataKind value used in BSP initialize params */
  val DataKind = "bleep-build"

  /** A resolved build, as sent to the server.
    *
    * @param variantName
    *   The build variant name (e.g., "normal", "bsp")
    * @param build
    *   The fully exploded build model
    * @param resolvedProjects
    *   Every project with its dependencies resolved — classpaths, compiler jars, source and output directories. This is what makes the server a pure executor:
    *   with these it never needs coursier or a `bleep.yaml` on the compile path.
    * @param buildId
    *   Content hash of the three fields above. Construct via [[Payload.of]] rather than passing this directly, so it cannot drift from what it identifies.
    */
  case class Payload(
      variantName: String,
      build: model.Build.Exploded,
      resolvedProjects: Map[model.CrossProjectName, ResolvedProject],
      buildId: BuildId
  )

  object Payload {

    /** Build a payload, deriving its [[BuildId]] from its contents. */
    def of(
        variantName: String,
        build: model.Build.Exploded,
        resolvedProjects: Map[model.CrossProjectName, ResolvedProject]
    ): Payload =
      Payload(variantName, build, resolvedProjects, buildIdOf(variantName, build, resolvedProjects))

    /** Everything the server needs to compile this build, taken from a client-side [[Started]].
      *
      * Forces every `Lazy[ResolvedProject]`, so the client pays for resolution once, up front, instead of the server repeating it.
      */
    def from(started: Started): Payload =
      of(
        variantName = started.buildPaths.variant.name,
        build = started.build.dropBuildFile,
        resolvedProjects = started.resolvedProjects.map { case (crossName, lazyResolved) => crossName -> lazyResolved.forceGet }
      )

    private implicit val mapEncoder: Encoder[Map[model.CrossProjectName, ResolvedProject]] =
      Encoder.encodeMap[model.CrossProjectName, ResolvedProject](using model.CrossProjectName.keyEncodes, ResolvedProject.encodes)
    private implicit val mapDecoder: Decoder[Map[model.CrossProjectName, ResolvedProject]] =
      Decoder.decodeMap[model.CrossProjectName, ResolvedProject](using model.CrossProjectName.keyDecodes, ResolvedProject.decodes)

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
        resolvedProjects: Map[model.CrossProjectName, ResolvedProject]
    ): BuildId = {
      import io.circe.syntax._
      BuildId.ofJson(
        Json.obj(
          "variantName" -> variantName.asJson,
          "build" -> build.asJson,
          "resolvedProjects" -> resolvedProjects.asJson
        )
      )
    }
  }
}
