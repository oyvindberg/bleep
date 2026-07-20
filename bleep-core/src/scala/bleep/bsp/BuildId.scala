package bleep.bsp

import io.circe.{Decoder, Encoder, Json, Printer}

import java.security.MessageDigest

/** Content hash identifying one resolved build.
  *
  * The client computes this over everything it ships in a [[BspBuildData.Payload]] and the server uses it to decide whether a cached build is still the one the
  * client means. Without it the server cannot tell "the same build again" (reuse the cache, skip resolution) from "a different build for the same workspace"
  * (adopt it, and say so) — it previously assumed the former and silently ignored the latter.
  */
case class BuildId(value: String) {

  /** For log lines. Full hashes make messages unreadable and the prefix is plenty to correlate two lines. */
  def short: String = value.take(12)

  override def toString: String = short
}

object BuildId {

  /** Sorted keys so the hash depends on the content, not on map iteration order or field-encoding order. Two processes encoding the same build must agree. */
  private val canonical: Printer = Printer.noSpaces.copy(sortKeys = true)

  def ofJson(json: Json): BuildId = {
    val digest = MessageDigest.getInstance("SHA-256").digest(canonical.print(json).getBytes("UTF-8"))
    BuildId(digest.map(b => f"${b & 0xff}%02x").mkString)
  }

  implicit val encoder: Encoder[BuildId] = Encoder[String].contramap(_.value)
  implicit val decoder: Decoder[BuildId] = Decoder[String].map(BuildId.apply)
}
