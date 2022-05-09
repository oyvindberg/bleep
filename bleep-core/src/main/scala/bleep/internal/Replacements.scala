package bleep.internal

import bleep.{Options, RelPath, Versions}

import java.nio.file.Path

// unfortunately we'll need to handle absolute paths in scalacOptions
class Replacements private (val map: List[(String, String)]) {
  def ++(other: Replacements): Replacements = Replacements.ofReplacements(map ++ other.map)

  class Replacer(replacements: List[(String, String)]) {
    def string(str: String): String =
      replacements.foldLeft(str) { case (acc, (from, to)) => acc.replace(from, to) }

    def relPath(relPath: RelPath): RelPath =
      new RelPath(relPath.segments.map(string))

    def opts(options: Options): Options =
      new Options(options.values.map {
        case Options.Opt.Flag(name)           => Options.Opt.Flag(string(name))
        case Options.Opt.WithArgs(name, args) => Options.Opt.WithArgs(string(name), args.map(string))
      })
  }

  object templatize extends Replacer(map)

  object fill extends Replacer(map.map { case (ref, absPath) => (absPath, ref) })
}

object Replacements {
  val empty: Replacements = ofReplacements(Nil)

  def ofReplacements(map: List[(String, String)]): Replacements =
    new Replacements(map.distinct.sortBy(-_._1.length)) // longest first

  def targetDir(target: Path): Replacements =
    ofReplacements(
      List(target.toString -> "${TARGET_DIR}")
    )

  def paths(build: Path, project: Path): Replacements =
    ofReplacements(
      List(
        project.toString -> "${PROJECT_DIR}",
        build.toString -> "${BUILD_DIR}"
      )
    )

  def versions(scalaVersion: Option[Versions.Scala], platform: Option[String]): Replacements =
    ofReplacements(
      List(
        scalaVersion match {
          case Some(scalaVersion) =>
            List(
//              scalaVersion.epoch.toString -> "${SCALA_EPOCH}",
              s"${scalaVersion.binVersion}" -> "${SCALA_BIN_VERSION}",
              scalaVersion.scalaVersion -> "${SCALA_VERSION}"
            )
          case None => Nil
        },
        platform match {
          case Some(platform) => List(s"$platform" -> "${PLATFORM}")
          case None           => Nil
        }
      ).flatten
    )
}
