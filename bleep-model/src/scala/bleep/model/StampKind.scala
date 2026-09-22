package bleep
package model

import io.circe.{Decoder, DecodingFailure, Encoder}

/** A value bleep derives from the build and writes into a project's artifact, under `stamp:` in `bleep.yaml`.
  *
  * Stamps exist because the obvious way to get a version into an artifact — a sourcegen writing it into a constant — puts it in the compile graph. A value
  * derived from git is not an input the generator watches, so it goes stale silently; and because generated sources are part of the cache key, a file that
  * changes on every commit makes its project and every project downstream of it a permanent cache miss. A version is data in the artifact, not an input to the
  * compiler. So a stamp is written to a resource directory that reaches the runtime classpath and the jar and nothing else ([[bleep.Usage.Runtime]]), and it is
  * recomputed on every build because it costs nothing to recompute something no cache key depends on.
  *
  *   - [[Dynver]]: which commit this was built from, as a publishable version — `1.0.0` on a clean tag, `1.0.0+4-abc1234` past one, with `-SNAPSHOT` and
  *     dirtiness exactly as `bleep publish` would derive it. It is the same derivation, and `bleep publish --version X` stamps X, so the published coordinate
  *     and the value inside the jar cannot disagree.
  *   - [[GitSha]]: which commit, always — including on a clean tag, where [[Dynver]] carries no sha. The support question: what exactly is running.
  *   - [[ProjectDigest]]: what is in this artifact. The project's content digest, the same one the remote cache keys on — so it changes if and only if what it
  *     describes changes, where a sha only says what was checked out. Not free: a plain compile computes no digests, so opting in adds one pass per build.
  *   - [[BuildDigest]]: which build produced this. The identity of the whole resolved build, already computed on every build; it correlates every artifact of
  *     one build even when the working tree is dirty, where [[Dynver]] cannot tell them apart.
  *
  * There is deliberately no timestamp: it would make every artifact byte-different and buys nothing the two identities above do not answer better.
  */
sealed abstract class StampKind(val value: String)

object StampKind {
  case object Dynver extends StampKind("dynver")
  case object GitSha extends StampKind("git-sha")
  case object ProjectDigest extends StampKind("project-digest")
  case object BuildDigest extends StampKind("build-digest")

  val All: List[StampKind] = List(Dynver, GitSha, ProjectDigest, BuildDigest)
  val byName: Map[String, StampKind] = All.map(x => x.value -> x).toMap

  def fromString(str: String): Either[String, StampKind] =
    byName.get(str).toRight(s"'$str' not among ${All.map(_.value).mkString(", ")}")

  implicit val ordering: Ordering[StampKind] = Ordering.by(_.value)

  implicit val decoder: Decoder[StampKind] =
    Decoder.instance { c =>
      c.as[String].flatMap(str => fromString(str).left.map(err => DecodingFailure(err, c.history)))
    }

  implicit val encoder: Encoder[StampKind] =
    Encoder.encodeString.contramap(_.value)
}
