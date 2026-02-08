package bleep.model

import bleep.internal.compat.*
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Kotlin configuration for a project.
  *
  * @param version
  *   The Kotlin version (e.g., "2.0.0", "2.1.0")
  * @param options
  *   Kotlin compiler options (e.g., "-Xjsr305=strict")
  * @param jvmTarget
  *   JVM bytecode target version (e.g., "11", "17", "21")
  * @param js
  *   Kotlin/JS specific configuration
  * @param native
  *   Kotlin/Native specific configuration
  */
case class Kotlin(
    version: Option[VersionKotlin],
    options: Options,
    jvmTarget: Option[String],
    js: Option[KotlinJs],
    native: Option[KotlinNative]
) extends SetLike[Kotlin] {

  override def intersect(other: Kotlin): Kotlin =
    Kotlin(
      version = if (version == other.version) version else None,
      options = options.intersect(other.options),
      jvmTarget = if (jvmTarget == other.jvmTarget) jvmTarget else None,
      js = js.zipCompat(other.js).map { case (a, b) => a.intersect(b) },
      native = native.zipCompat(other.native).map { case (a, b) => a.intersect(b) }
    )

  override def removeAll(other: Kotlin): Kotlin =
    Kotlin(
      version = if (version == other.version) None else version,
      options = options.removeAll(other.options),
      jvmTarget = if (jvmTarget == other.jvmTarget) None else jvmTarget,
      js = removeAllFrom(js, other.js),
      native = removeAllFrom(native, other.native)
    )

  override def union(other: Kotlin): Kotlin =
    Kotlin(
      version = version.orElse(other.version),
      options = options.union(other.options),
      jvmTarget = jvmTarget.orElse(other.jvmTarget),
      js = List(js, other.js).flatten.reduceOption(_ union _),
      native = List(native, other.native).flatten.reduceOption(_ union _)
    )

  override def isEmpty: Boolean =
    version.isEmpty && options.isEmpty && jvmTarget.isEmpty &&
      js.forall(_.isEmpty) && native.forall(_.isEmpty)

  /** Helper to remove all SetLike elements */
  private def removeAllFrom[T <: SetLike[T]](a: Option[T], b: Option[T]): Option[T] =
    (a, b) match {
      case (Some(aVal), Some(bVal)) => Some(aVal.removeAll(bVal)).filterNot(_.isEmpty)
      case _                        => a
    }
}

object Kotlin {
  val empty: Kotlin = Kotlin(
    version = None,
    options = Options.empty,
    jvmTarget = None,
    js = None,
    native = None
  )

  implicit val decodes: Decoder[Kotlin] = deriveDecoder
  implicit val encodes: Encoder[Kotlin] = deriveEncoder
}
