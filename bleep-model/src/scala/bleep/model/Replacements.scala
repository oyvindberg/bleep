package bleep.model

import bleep.RelPath

import java.io.File
import java.nio.file.Path

// unfortunately we'll need to handle absolute paths in scalacOptions
class Replacements private (val sortedValues: List[(String, String)]) {
  def ++(other: Replacements): Replacements = Replacements.ofReplacements(sortedValues ++ other.sortedValues)

  object templatize extends Replacements.Replacer {
    // only infer templates on word and/or symbol boundary
    override def string(_str: String): String = {
      var str = _str
      var i = 0
      while (i < sortedValues.length) {
        val (from, to) = sortedValues(i)
        var last = 0
        while (last < str.length)
          str.indexOf(from, last) match {
            case -1 => last = str.length
            case n =>
              val before = str.substring(0, n)
              val after = str.substring(n + from.length)

              def beforeEndsWithSpecial = before.lastOption.exists(!_.isLetterOrDigit)

              def afterStartsWithSpecial = after.headOption.exists(!_.isLetterOrDigit)

              def beforeOk = before.isEmpty || beforeEndsWithSpecial

              def afterOk = after.isEmpty || afterStartsWithSpecial

              val doReplacement = beforeOk && afterOk
              if (doReplacement) {
                str = before + to + after
                last = n + to.length
              } else {
                last = last + 1
              }
          }
        i += 1
      }
      str
    }
  }

  object fill extends Replacements.Replacer {
    val replacements = sortedValues.map { case (ref, absPath) => (absPath, ref) }

    // unconditionally replace template strings with the real values
    override def string(str: String): String =
      replacements.foldLeft(str) { case (acc, (from, to)) => acc.replace(from, to) }
  }
}

object Replacements {
  @FunctionalInterface
  trait Replacer {
    def string(str: String): String

    def relPath(relPath: RelPath): RelPath =
      new RelPath(relPath.segments.map(string))

    def path(path: Path): Path =
      Path.of(string(path.toString))
    def file(file: File): File =
      new File(string(file.toString))

    def dep(dep: Dep): Dep = dep.withVersion(string(dep.version))

    def opts(options: Options): Options =
      new Options(options.values.map {
        case Options.Opt.Flag(name)           => Options.Opt.Flag(string(name))
        case Options.Opt.WithArgs(name, args) => Options.Opt.WithArgs(string(name), args.map(string))
      })
  }

  val empty: Replacements = ofReplacements(Nil)

  object known {
    val BleepVersion = "${BLEEP_VERSION}"
    val BuildDir = "${BUILD_DIR}"
    val Platform = "${PLATFORM}"
    val PlatformVersion = "${PLATFORM_VERSION}"
    val ProjectDir = "${PROJECT_DIR}"
    val ScalaBinVersion = "${SCALA_BIN_VERSION}"
    val ScalaEpoch = "${SCALA_EPOCH}"
    val ScalaVersion = "${SCALA_VERSION}"
    val Scope = "${SCOPE}"
    val TargetDir = "${TARGET_DIR}"
  }

  def ofReplacements(map: List[(String, String)]): Replacements =
    new Replacements(map.distinct.sortBy(-_._1.length)) // longest first

  def targetDir(target: Path): Replacements =
    ofReplacements(
      List(target.toString -> known.TargetDir)
    )

  def paths(build: Path, project: Path): Replacements =
    ofReplacements(
      List(
        project.toString -> known.ProjectDir,
        build.toString -> known.BuildDir
      )
    )

  def scope(scope: String): Replacements =
    ofReplacements(List(scope -> known.Scope))

  def versions(
      bleepVersion: Option[BleepVersion],
      scalaVersion: Option[VersionScala],
      platform: Option[PlatformId],
      platformVersion: Option[String],
      includeEpoch: Boolean,
      includeBinVersion: Boolean
  ): Replacements =
    ofReplacements(
      List(
        scalaVersion match {
          case Some(scalaVersion) =>
            List(
              (scalaVersion.epoch.toString, if (includeEpoch) Some(known.ScalaEpoch) else None),
              (scalaVersion.binVersion, if (includeBinVersion) Some(known.ScalaBinVersion) else None),
              (scalaVersion.scalaVersion, Some(known.ScalaVersion))
            ).collect { case (k, Some(v)) => (k, v) }
          case None => Nil
        },
        platform match {
          case Some(platform) => List(platform.value -> known.Platform)
          case None           => Nil
        },
        platformVersion match {
          case Some(version) =>
            List(version -> known.PlatformVersion)
          case None => Nil
        },
        bleepVersion match {
          case Some(value) => List(value.value -> known.BleepVersion)
          case None        => Nil
        }
      ).flatten
    )

  // note: bleepVersion may not be the running version in the case of `--dev`
  // where the version found in the build is used instead.
  // this is so that bleep-task dependencies can be resolved.
  def versions(bleepVersion: Option[BleepVersion], versionCombo: VersionCombo, includeEpoch: Boolean, includeBinVersion: Boolean): Replacements =
    versions(
      bleepVersion,
      scalaVersion = versionCombo.asScala.map(_.scalaVersion),
      platform = versionCombo match {
        case VersionCombo.Java         => None
        case VersionCombo.Jvm(_)       => Some(PlatformId.Jvm)
        case VersionCombo.Js(_, _)     => Some(PlatformId.Js)
        case VersionCombo.Native(_, _) => Some(PlatformId.Native)
      },
      platformVersion = versionCombo match {
        case VersionCombo.Java                   => None
        case VersionCombo.Jvm(_)                 => None
        case VersionCombo.Js(_, scalaJsVersion)  => Some(scalaJsVersion.scalaJsVersion)
        case VersionCombo.Native(_, scalaNative) => Some(scalaNative.scalaNativeVersion)
      },
      includeEpoch = includeEpoch,
      includeBinVersion = includeBinVersion
    )
}
