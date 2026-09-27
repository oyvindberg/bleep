package bleep.model

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

/** A program that rewrites this project's compiled classes before anything else sees them.
  *
  * The compiler writes into a private directory (`classes-pre`). After every compile that changed it, bleep forks `main` from `project` with the compiler's
  * output as `--from` (read-only) and an empty directory as `--to`, which the program fills with the project's *complete* output: whatever it copies, rewrites
  * or adds. Only a successful run is published into the project's classes directory, which is all that consumers, runners and packaging ever see.
  *
  * @param project
  *   the project whose `main` does the rewriting — built first, never on this project's classpath
  * @param inputs
  *   other projects whose output the program needs; built first, handed over as `--input <name>=<classes dir>`, never on this project's classpath
  */
case class PostCompile(project: CrossProjectName, main: String, inputs: JsonSet[CrossProjectName])

object PostCompile {
  implicit val codec: Codec.AsObject[PostCompile] = deriveCodec
}
