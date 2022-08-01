package bleep.internal

import bleep.{Options, RelPath, Versions}

import java.nio.file.Path

// unfortunately we'll need to handle absolute paths in scalacOptions
class Replacements private (val sortedValues: List[(String, String)]) {
  def ++(other: Replacements): Replacements = Replacements.ofReplacements(sortedValues ++ other.sortedValues)

  trait Replacer {
    def string(str: String): String

    def relPath(relPath: RelPath): RelPath =
      new RelPath(relPath.segments.map(string))

    def opts(options: Options): Options =
      new Options(options.values.map {
        case Options.Opt.Flag(name)           => Options.Opt.Flag(string(name))
        case Options.Opt.WithArgs(name, args) => Options.Opt.WithArgs(string(name), args.map(string))
      })
  }

  object templatize extends Replacer {
    // only infer templates on word and/or symbol boundary
    override def string(_str: String): String = {
      var str = _str
      var i = 0
      while (i < sortedValues.length) {
        val (from, to) = sortedValues(i)
        str.indexOf(from) match {
          case -1 => ()
          case n =>
            val before = str.substring(0, n)
            val after = str.substring(n + from.length)
            val beforeEndsWithSpecial = before.lastOption.exists(!_.isLetterOrDigit)
            val afterStartsWithSpecial = after.headOption.exists(!_.isLetterOrDigit)
            val beforeOk = before.isEmpty || beforeEndsWithSpecial
            val afterOk = after.isEmpty || afterStartsWithSpecial
            val doReplacement = beforeOk && afterOk
            if (doReplacement) {
              str = before + to + after
              // neutralize increment below - run replacement again
              i -= 1
            }
        }
        i += 1
      }
      str
    }
  }

  object fill extends Replacer {
    val replacements = sortedValues.map { case (ref, absPath) => (absPath, ref) }

    // unconditionally replace template strings with the real values
    override def string(str: String): String =
      replacements.foldLeft(str) { case (acc, (from, to)) => acc.replace(from, to) }
  }
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

  def scope(scope: String): Replacements =
    if (scope.isEmpty) empty else ofReplacements(List(scope -> "${SCOPE}"))

  def versions(scalaVersion: Option[Versions.Scala], platform: Option[String], includeEpoch: Boolean): Replacements =
    ofReplacements(
      List(
        scalaVersion match {
          case Some(scalaVersion) =>
            List(
              scalaVersion.epoch.toString -> (if (includeEpoch) Some("${SCALA_EPOCH}") else None),
              scalaVersion.binVersion -> Some("${SCALA_BIN_VERSION}"),
              scalaVersion.scalaVersion -> Some("${SCALA_VERSION}")
            ).collect { case (k, Some(v)) => (k, v) }
          case None => Nil
        },
        platform match {
          case Some(platform) => List(s"$platform" -> "${PLATFORM}")
          case None           => Nil
        }
      ).flatten
    )
}
