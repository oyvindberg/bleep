package bleep

import io.circe.{Decoder, Encoder}

/** Which question a consumer of a project's directories is asking.
  *
  * Bleep keeps every kind of source and resource directory in the same collections — hand-written ones, generator output, annotation-processor output, and
  * values derived from the build itself — and for most of its history every consumer wanted all of them. They no longer agree. Three questions are asked, and
  * each is answered by a different set:
  *
  *   - [[Usage.Input]]: *does changing this change what the build produces?* Only content a person or a free-standing generator wrote. This is the set that is
  *     hashed into the cache key, invalidates a project, wakes `--watch`, and makes a sourcegen stale.
  *   - [[Usage.Compile]]: *does the compiler see this?* The inputs, plus what a compile-time generator expanded into files along the way.
  *   - [[Usage.Runtime]]: *does this have to exist when the code runs or ships?* Everything the compiler sees, plus values bleep derives from the build and
  *     writes into the artifact.
  *
  * The three are nested — `Input ⊆ Compile ⊆ Runtime` — and every origin in [[ProjectPaths.DirsByOrigin]] is tagged at the narrowest one that includes it.
  * Asking at a wider level returns everything tagged at or below it.
  *
  * There is no default. Picking one is the point, and a fourth value would break every exhaustive match below this one, which is how the compiler hands you the
  * list of decisions to revisit instead of silently choosing.
  *
  * ==Why annotation-processor output is not an input==
  *
  * It is a function of the sources and the processor coordinates, both of which are already inputs — the same as a Scala macro's expansion, which bleep has
  * never hashed because it is never written anywhere. A processor that could read something outside those (a file, an environment variable) could equally be a
  * macro doing so; the digest already trusts compile-time generators to be pure, and treating the one that happens to use a file as its intermediate
  * differently bought nothing but a failure mode: any processor that embeds a timestamp moved the digest on every build and left its project and everything
  * downstream permanently cold.
  *
  * ==Why sourcegen output IS an input==
  *
  * Because a sourcegen is the tool people reach for precisely when they need to read outside the source tree — a schema, the git history, the network — which
  * is why it can declare `sourceGlobs`. Hashing its output is what turns a generator reading something bleep does not know about into a cache miss, instead of
  * a cache hit serving classes compiled from a stale value.
  */
sealed trait Usage {

  /** Whether asking at this usage returns an entry tagged at `tagged` — true when `tagged` is this tier or a narrower one. */
  final def admits(tagged: Usage): Boolean = Usage.rank(tagged) <= Usage.rank(this)
}

object Usage {

  /** Content here decides what the build produces: hashed into [[ProjectDigest]]'s cache key, invalidates the project for `bleep build invalidated`, wakes
    * `--watch`, and makes a sourcegen stale.
    */
  case object Input extends Usage

  /** [[Input]] plus what a compile-time generator — an annotation processor, KSP — expanded into files. The compiler sees it; nothing is keyed on it. */
  case object Compile extends Usage

  /** [[Compile]] plus whatever only has to exist when the code runs or ships: on a runtime classpath, or inside a jar. */
  case object Runtime extends Usage

  /** Position in `Input ⊆ Compile ⊆ Runtime`. Exhaustive on purpose: a fourth usage cannot compile until someone has decided where it sits. */
  private[bleep] def rank(usage: Usage): Int = usage match {
    case Input   => 0
    case Compile => 1
    case Runtime => 2
  }

  // Crosses the wire inside `ResolvedProject`, so the spelling is part of the client/server protocol.
  implicit val encodes: Encoder[Usage] = Encoder[String].contramap {
    case Input   => "input"
    case Compile => "compile"
    case Runtime => "runtime"
  }
  implicit val decodes: Decoder[Usage] = Decoder[String].emap {
    case "input"   => Right(Input)
    case "compile" => Right(Compile)
    case "runtime" => Right(Runtime)
    case other     => Left(s"unknown usage '$other', expected one of input, compile, runtime")
  }
}
