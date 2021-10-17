package bleep

import bleep.internal.EnumDecoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.{Decoder, DecodingFailure, KeyDecoder}

object model {

  implicit val decodesScala: Decoder[Versions.Scala] = Decoder[String].map(Versions.Scala.apply)

  case class Java(options: Option[JsonList[String]])

  object Java {
    implicit val decodes: Decoder[Java] = deriveDecoder

    def merge(xs: Option[Java]*): Option[Java] =
      xs.flatten.reduceOption((j1, j2) =>
        Java(
          options = j1.options.orElse(j2.options)
        )
      )
  }

  case class TestFramework(names: List[String])

  object TestFramework {
    implicit val decodes: Decoder[TestFramework] = deriveDecoder
  }
  case class TestArgument(args: List[String], framework: Option[TestFramework])

  object TestArgument {
    implicit val decodes: Decoder[TestArgument] = deriveDecoder
  }

  case class TestOptions(excludes: List[String], arguments: List[TestArgument])

  object TestOptions {
    implicit val decodes: Decoder[TestOptions] = deriveDecoder
  }

  case class Test(frameworks: List[TestFramework], options: TestOptions)

  object Test {
    implicit val decodes: Decoder[Test] = deriveDecoder
  }

  sealed abstract class CompileOrder(val id: String)

  object CompileOrder extends EnumDecoder[CompileOrder] {
    case object Mixed extends CompileOrder("mixed")
    case object JavaThenScala extends CompileOrder("java->scala")
    case object ScalaThenJava extends CompileOrder("scala->java")

    final val All: Map[String, CompileOrder] =
      List(Mixed, JavaThenScala, ScalaThenJava).map(x => x.id -> x).toMap
  }

  case class CompileSetup(
      order: Option[CompileOrder],
      addLibraryToBootClasspath: Option[Boolean],
      addCompilerToClasspath: Option[Boolean],
      addExtraJarsToClasspath: Option[Boolean],
      manageBootClasspath: Option[Boolean],
      filterLibraryFromClasspath: Option[Boolean]
  )

  object CompileSetup {
    def merge(xs: Option[CompileSetup]*): Option[CompileSetup] =
      xs.flatten.reduceOption((s1, s2) =>
        CompileSetup(
          order = s1.order.orElse(s2.order),
          addLibraryToBootClasspath = s1.addLibraryToBootClasspath.orElse(s2.addLibraryToBootClasspath),
          addCompilerToClasspath = s1.addCompilerToClasspath.orElse(s2.addCompilerToClasspath),
          addExtraJarsToClasspath = s1.addExtraJarsToClasspath.orElse(s2.addExtraJarsToClasspath),
          manageBootClasspath = s1.manageBootClasspath.orElse(s2.manageBootClasspath),
          filterLibraryFromClasspath = s1.filterLibraryFromClasspath.orElse(s2.filterLibraryFromClasspath)
        )
      )
    implicit val decodes: Decoder[CompileSetup] = deriveDecoder
  }

  case class Scala(
      version: Option[Versions.Scala],
      options: Option[JsonList[String]],
      setup: Option[CompileSetup]
  )

  object Scala {
    implicit val decodes: Decoder[Scala] = deriveDecoder

    def merge(xs: Option[Scala]*): Option[Scala] =
      xs.flatten.reduceOption { (s1, s2) =>
        Scala(
          version = s1.version.orElse(s2.version),
          options = s1.options.orElse(s2.options),
          setup = CompileSetup.merge(s1.setup, s2.setup)
        )
      }
  }

  sealed abstract class Platform(val name: String, val config: PlatformConfig, val mainClass: Option[String])

  object Platform {

    case class Js(override val config: JsConfig, override val mainClass: Option[String]) extends Platform("js", config, mainClass)

    case class Jvm(
        override val config: JvmConfig,
        override val mainClass: Option[String],
        runtimeConfig: Option[JvmConfig]
        //        classpath: Option[List[Path]],
        //        resources: Option[List[Path]]
    ) extends Platform("jvm", config, mainClass)

    case class Native(override val config: NativeConfig, override val mainClass: Option[String]) extends Platform("native", config, mainClass)

    implicit val decodesJs: Decoder[Js] = deriveDecoder[Js]
    implicit val decodesJvm: Decoder[Jvm] = deriveDecoder[Jvm]
    implicit val decodesNative: Decoder[Native] = deriveDecoder[Native]

    implicit val decodes: Decoder[Platform] = Decoder.instance(c =>
      for {
        name <- c.get[String]("name")
        platform <- name match {
          case "jvm"    => decodesJvm(c)
          case "js"     => decodesJs(c)
          case "native" => decodesNative(c)
          case other    => Left(DecodingFailure(s"$other is not a valid platform", c.history))
        }
      } yield platform
    )
  }

  sealed trait PlatformConfig

  object PlatformConfig {
    implicit val decodesJvm: Decoder[JvmConfig] = deriveDecoder
    implicit val decodesJs: Decoder[JsConfig] = deriveDecoder
    implicit val decodesNative: Decoder[NativeConfig] = deriveDecoder
  }
  case class JvmConfig(
      //      home: Option[Path],
      options: List[String]
  ) extends PlatformConfig

  sealed abstract class LinkerMode(val id: String)

  object LinkerMode extends EnumDecoder[LinkerMode] {
    case object Debug extends LinkerMode("debug")
    case object Release extends LinkerMode("release")
    val All = List(Debug, Release).map(x => x.id -> x).toMap
  }

  sealed abstract class ModuleKindJS(val id: String)

  object ModuleKindJS extends EnumDecoder[ModuleKindJS] {
    case object NoModule extends ModuleKindJS("none")
    case object CommonJSModule extends ModuleKindJS("commonjs")
    case object ESModule extends ModuleKindJS("esmodule")
    val All = List(NoModule, CommonJSModule, ESModule).map(x => x.id -> x).toMap
  }

  case class JsConfig(
      version: String,
      mode: LinkerMode,
      kind: ModuleKindJS,
      emitSourceMaps: Boolean,
      jsdom: Option[Boolean],
      output: Option[RelPath]
      //      nodePath: Option[Path],
      //      toolchain: List[Path]
  ) extends PlatformConfig

  case class NativeConfig(
      version: String,
      mode: LinkerMode,
      gc: String,
      targetTriple: Option[String],
      //      clang: Path,
      //      clangpp: Path,
      //      toolchain: List[Path],
      options: NativeOptions,
      linkStubs: Boolean,
      check: Boolean,
      dump: Boolean
      //      output: Option[Path]
  ) extends PlatformConfig

  case class NativeOptions(linker: List[String], compiler: List[String])

  object NativeOptions {
    implicit val decodes: Decoder[NativeOptions] = deriveDecoder
  }

  case class ProjectName(value: String) extends AnyVal

  object ProjectName {
    implicit val decodes: Decoder[ProjectName] = Decoder[String].map(ProjectName.apply)
    implicit val keyDecodes: KeyDecoder[ProjectName] = KeyDecoder[String].map(ProjectName.apply)
  }

  case class Project(
      folder: Option[RelPath],
      dependsOn: Option[JsonList[ProjectName]],
      sources: Option[JsonList[RelPath]],
      resources: Option[JsonList[RelPath]],
      dependencies: Option[JsonList[Dep]],
      java: Option[Java],
      scala: Option[Scala],
      platform: Option[Platform]
  )

  object Project {
    implicit val decodes: Decoder[Project] = deriveDecoder
  }

  case class File(
      version: String,
      scala: Option[Scala],
      java: Option[Java],
      projects: Map[ProjectName, Project]
  )

  object File {
    implicit val decodes: Decoder[File] = deriveDecoder
  }
}
