package bleep.model

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import bleep.internal.compat.OptionCompatOps

case class Scala(
    version: Option[VersionScala],
    options: Options,
    setup: Option[CompileSetup],
    compilerPlugins: JsonSet[Dep],
    strict: Option[Boolean]
) extends SetLike[Scala] {
  override def intersect(other: Scala): Scala =
    Scala(
      version = if (`version` == other.`version`) `version` else None,
      options = options.intersect(other.options),
      setup = setup.zipCompat(other.setup).map { case (_1, _2) => _1.intersect(_2) },
      compilerPlugins = compilerPlugins.intersect(other.compilerPlugins),
      strict = if (strict == other.strict) strict else None
    )

  override def removeAll(other: Scala): Scala =
    Scala(
      version = if (`version` == other.`version`) None else `version`,
      options = options.removeAll(other.options),
      setup = removeAllFrom(setup, other.setup),
      compilerPlugins = compilerPlugins.removeAll(other.compilerPlugins),
      strict = if (strict == other.strict) None else strict
    )

  override def union(other: Scala): Scala =
    Scala(
      version = version.orElse(other.version),
      options = options.union(other.options),
      setup = List(setup, other.setup).flatten.reduceOption(_ union _),
      compilerPlugins = compilerPlugins.union(other.compilerPlugins),
      strict = strict.orElse(other.strict)
    )

  override def isEmpty: Boolean =
    this match {
      case Scala(version, options, setup, compilerPlugins, strict) =>
        version.isEmpty && options.isEmpty && setup.fold(true)(_.isEmpty) && compilerPlugins.isEmpty && strict.isEmpty
    }
}

object Scala {
  implicit val decodes: Decoder[Scala] = deriveDecoder
  implicit val encodes: Encoder[Scala] = deriveEncoder
}
