package bleep.analysis

import java.nio.file.Path

/** Configuration for Scala.js linking.
  *
  * Contains all options needed to configure the Scala.js linker.
  */
case class ScalaJsLinkConfig(
    mode: ScalaJsLinkConfig.LinkerMode,
    moduleKind: ScalaJsLinkConfig.ModuleKind,
    moduleSplitStyle: ScalaJsLinkConfig.ModuleSplitStyle,
    emitSourceMaps: Boolean,
    jsHeader: Option[String],
    minify: Boolean,
    prettyPrint: Boolean,
    esFeatures: ScalaJsLinkConfig.EsFeatures,
    checkIR: Boolean,
    optimizer: Boolean
)

object ScalaJsLinkConfig {

  /** Linker mode - determines optimization level.
    */
  sealed trait LinkerMode {
    def name: String
  }
  object LinkerMode {
    case object Debug extends LinkerMode { val name = "debug" }
    case object Release extends LinkerMode { val name = "release" }
  }

  /** JavaScript module kind for output.
    */
  sealed trait ModuleKind {
    def name: String
  }
  object ModuleKind {
    case object NoModule extends ModuleKind { val name = "NoModule" }
    case object CommonJSModule extends ModuleKind { val name = "CommonJSModule" }
    case object ESModule extends ModuleKind { val name = "ESModule" }
  }

  /** Module split style for code splitting.
    */
  sealed trait ModuleSplitStyle {
    def name: String
  }
  object ModuleSplitStyle {
    case object FewestModules extends ModuleSplitStyle { val name = "FewestModules" }
    case object SmallestModules extends ModuleSplitStyle { val name = "SmallestModules" }
    case class SmallModulesFor(packages: List[String]) extends ModuleSplitStyle {
      val name = "SmallModulesFor"
    }
  }

  /** ECMAScript features to enable/disable.
    */
  case class EsFeatures(
      esVersion: EsVersion,
      useECMAScript2015Semantics: Boolean,
      avoidClasses: Boolean,
      avoidLetsAndConsts: Boolean,
      allowBigIntsForLongs: Boolean
  )

  object EsFeatures {
    val Defaults: EsFeatures = EsFeatures(
      esVersion = EsVersion.ES2015,
      useECMAScript2015Semantics = true,
      avoidClasses = false,
      avoidLetsAndConsts = false,
      allowBigIntsForLongs = false
    )
  }

  /** ECMAScript version.
    */
  sealed trait EsVersion {
    def year: Int
  }
  object EsVersion {
    case object ES5_1 extends EsVersion { val year = 5 }
    case object ES2015 extends EsVersion { val year = 2015 }
    case object ES2016 extends EsVersion { val year = 2016 }
    case object ES2017 extends EsVersion { val year = 2017 }
    case object ES2018 extends EsVersion { val year = 2018 }
    case object ES2019 extends EsVersion { val year = 2019 }
    case object ES2020 extends EsVersion { val year = 2020 }
    case object ES2021 extends EsVersion { val year = 2021 }
  }

  /** Default debug configuration. */
  val Debug: ScalaJsLinkConfig = ScalaJsLinkConfig(
    mode = LinkerMode.Debug,
    moduleKind = ModuleKind.CommonJSModule,
    moduleSplitStyle = ModuleSplitStyle.FewestModules,
    emitSourceMaps = true,
    jsHeader = None,
    minify = false,
    prettyPrint = false,
    esFeatures = EsFeatures.Defaults,
    checkIR = false,
    optimizer = false
  )

  /** Default release configuration. */
  val Release: ScalaJsLinkConfig = ScalaJsLinkConfig(
    mode = LinkerMode.Release,
    moduleKind = ModuleKind.CommonJSModule,
    moduleSplitStyle = ModuleSplitStyle.FewestModules,
    emitSourceMaps = false,
    jsHeader = None,
    minify = true,
    prettyPrint = false,
    esFeatures = EsFeatures.Defaults,
    checkIR = false,
    optimizer = true
  )
}

/** Result of Scala.js linking.
  */
case class ScalaJsLinkResult(
    outputFiles: Seq[Path],
    mainModule: Path,
    publicModules: Seq[Path]
) {
  def isSuccess: Boolean = outputFiles.nonEmpty
}
