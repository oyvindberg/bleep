package bleep
package model

import bleep.internal.compat.OptionCompatOps
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** @param skipStdlib
  *   no Scala standard library from a repository — neither the one bleep adds nor one a dependency brings in transitively. For the project that *is* the
  *   standard library, and for projects that get it through `dependsOn` on such a project.
  * @param compilerProject
  *   compile with the Scala compiler this project builds, instead of fetching `version` from a repository. The named project's runtime classpath is the
  *   compiler, and its own classes are the compiler bridge — so name the bridge project, which depends on the compiler. An [[IndirectDependency]]: built first,
  *   never on this project's classpath.
  */
case class Scala(
    version: Option[VersionScala],
    options: Options,
    setup: Option[CompileSetup],
    compilerPlugins: JsonSet[Dep],
    strict: Option[Boolean],
    skipStdlib: Option[Boolean],
    compilerProject: Option[CrossProjectName],
    /** an sbt plugin: for sbt 1 when the scala version is 2.12, for sbt 2 when it is 3. Published under the name sbt looks plugins up by, see
      * [[Scala.SbtPlugin]]
      */
    sbtPlugin: Option[Boolean]
) extends SetLike[Scala] {
  override def intersect(other: Scala): Scala =
    Scala(
      version = if (`version` == other.`version`) `version` else None,
      options = options.intersect(other.options),
      setup = setup.zipCompat(other.setup).map { case (_1, _2) => _1.intersect(_2) },
      compilerPlugins = compilerPlugins.intersect(other.compilerPlugins),
      strict = if (strict == other.strict) strict else None,
      skipStdlib = if (skipStdlib == other.skipStdlib) skipStdlib else None,
      compilerProject = if (compilerProject == other.compilerProject) compilerProject else None,
      sbtPlugin = if (sbtPlugin == other.sbtPlugin) sbtPlugin else None
    )

  override def removeAll(other: Scala): Scala =
    Scala(
      version = if (`version` == other.`version`) None else `version`,
      options = options.removeAll(other.options),
      setup = removeAllFrom(setup, other.setup),
      compilerPlugins = compilerPlugins.removeAll(other.compilerPlugins),
      strict = if (strict == other.strict) None else strict,
      skipStdlib = if (skipStdlib == other.skipStdlib) None else skipStdlib,
      compilerProject = if (compilerProject == other.compilerProject) None else compilerProject,
      sbtPlugin = if (sbtPlugin == other.sbtPlugin) None else sbtPlugin
    )

  override def union(other: Scala): Scala =
    Scala(
      version = version.orElse(other.version),
      options = options.union(other.options),
      setup = List(setup, other.setup).flatten.reduceOption(_ `union` _),
      compilerPlugins = compilerPlugins.union(other.compilerPlugins),
      strict = strict.orElse(other.strict),
      skipStdlib = skipStdlib.orElse(other.skipStdlib),
      compilerProject = compilerProject.orElse(other.compilerProject),
      sbtPlugin = sbtPlugin.orElse(other.sbtPlugin)
    )

  override def isEmpty: Boolean =
    this match {
      case Scala(version, options, setup, compilerPlugins, strict, skipStdlib, compilerProject, sbtPlugin) =>
        version.isEmpty && options.isEmpty && setup.fold(true)(_.isEmpty) && compilerPlugins.isEmpty && strict.isEmpty && skipStdlib.isEmpty &&
        compilerProject.isEmpty && sbtPlugin.isEmpty
    }
}

object Scala {

  /** Which sbt a plugin is for, which its scala version says: sbt 1 plugins are scala 2.12, sbt 2 plugins scala 3 */
  sealed abstract class SbtPlugin(val sbtMajor: Int, val sbtBinaryVersion: String) {

    /** the name a plugin is published and looked up under: `sbt-ci-release_2.12_1.0`, `sbt-ci-release_sbt2_3` */
    def artifactName(name: String): String
  }

  object SbtPlugin {
    case object Sbt1 extends SbtPlugin(sbtMajor = 1, sbtBinaryVersion = "1.0") {
      def artifactName(name: String): String = s"${name}_2.12_1.0"
    }
    case object Sbt2 extends SbtPlugin(sbtMajor = 2, sbtBinaryVersion = "2") {
      def artifactName(name: String): String = s"${name}_sbt2_3"
    }

    def forScalaVersion(scalaVersion: VersionScala): Either[String, SbtPlugin] =
      if (scalaVersion.is3) Right(Sbt2)
      else if (scalaVersion.binVersion == "2.12") Right(Sbt1)
      else Left(s"sbt plugins are scala 2.12 (sbt 1) or scala 3 (sbt 2), not ${scalaVersion.scalaVersion}")
  }

  implicit val decodes: Decoder[Scala] = deriveDecoder
  implicit val encodes: Encoder[Scala] = deriveEncoder
}
