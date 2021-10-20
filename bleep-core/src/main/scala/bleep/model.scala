package bleep

import bleep.internal.EnumCodec
import bloop.config.Config
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe._
import Dep.{decodesDep, encodesDep}
import coursier.parse.JavaOrScalaDependency

import java.net.URI

object model {

  implicit val decodesScala: Decoder[Versions.Scala] = Decoder[String].map(Versions.Scala.apply)
  implicit val encodesScala: Encoder[Versions.Scala] = Encoder[String].contramap(_.scalaVersion)
  implicit val decodesURI: Decoder[URI] = Decoder[String].emap(str =>
    try Right(URI.create(str))
    catch { case x: IllegalArgumentException => Left(x.getMessage) }
  )
  implicit val encodesURI: Encoder[URI] = Encoder[String].contramap(_.toString)

  case class Java(options: Option[JsonList[String]])

  object Java {
    implicit val decodes: Decoder[Java] = deriveDecoder
    implicit val encodes: Encoder[Java] = deriveEncoder

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
    implicit val encodes: Encoder[TestFramework] = deriveEncoder
  }
  case class TestArgument(args: List[String], framework: Option[TestFramework])

  object TestArgument {
    implicit val decodes: Decoder[TestArgument] = deriveDecoder
    implicit val encodes: Encoder[TestArgument] = deriveEncoder
  }

  case class TestOptions(excludes: List[String], arguments: List[TestArgument])

  object TestOptions {
    implicit val decodes: Decoder[TestOptions] = deriveDecoder
    implicit val encodes: Encoder[TestOptions] = deriveEncoder
  }

  case class Test(frameworks: List[TestFramework], options: TestOptions)

  object Test {
    implicit val decodes: Decoder[Test] = deriveDecoder
    implicit val encodes: Encoder[Test] = deriveEncoder
  }

  object CompileOrder extends EnumCodec[Config.CompileOrder] {
    final val All: Map[String, Config.CompileOrder] =
      List(Config.Mixed, Config.JavaThenScala, Config.ScalaThenJava).map(x => x.id -> x).toMap
  }
  import CompileOrder.{decodesEnum, encodesEnum}

  case class CompileSetup(
      order: Option[Config.CompileOrder],
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
    implicit val encodes: Encoder[CompileSetup] = deriveEncoder
  }

  case class Scala(
      version: Option[Versions.Scala],
      options: Option[JsonList[String]],
      setup: Option[CompileSetup]
  )

  object Scala {
    implicit val decodes: Decoder[Scala] = deriveDecoder
    implicit val encodes: Encoder[Scala] = deriveEncoder

    def merge(xs: Option[Scala]*): Option[Scala] =
      xs.flatten.reduceOption { (s1, s2) =>
        Scala(
          version = s1.version.orElse(s2.version),
          options = s1.options.orElse(s2.options),
          setup = CompileSetup.merge(s1.setup, s2.setup)
        )
      }
  }

  sealed abstract class Platform(val name: String, val config: Option[PlatformConfig], val mainClass: Option[String])

  object Platform {

    case class Js(override val config: Option[JsConfig], override val mainClass: Option[String]) extends Platform("js", config, mainClass)

    case class Jvm(
        override val config: Option[JvmConfig],
        override val mainClass: Option[String],
        runtimeConfig: Option[JvmConfig]
        //        classpath: Option[List[Path]],
        //        resources: Option[List[Path]]
    ) extends Platform("jvm", config, mainClass)

    case class Native(override val config: Option[NativeConfig], override val mainClass: Option[String]) extends Platform("native", config, mainClass)

    implicit val decodesJs: Decoder[Js] = deriveDecoder
    implicit val encodesJs: Encoder[Js] = deriveEncoder
    implicit val decodesJvm: Decoder[Jvm] = deriveDecoder
    implicit val encodesJvm: Encoder[Jvm] = deriveEncoder
    implicit val decodesNative: Decoder[Native] = deriveDecoder
    implicit val encodesNative: Encoder[Native] = deriveEncoder

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

    implicit val encodes: Encoder[Platform] = Encoder.instance { platform =>
      val obj = platform match {
        case x: Js     => encodesJs(x)
        case x: Jvm    => encodesJvm(x)
        case x: Native => encodesNative(x)
      }
      obj.mapObject(_.add("name", Json.fromString(platform.name)))
    }
  }

  sealed trait PlatformConfig

  object PlatformConfig {
    implicit val decodesJvm: Decoder[JvmConfig] = deriveDecoder
    implicit val encodesJvm: Encoder[JvmConfig] = deriveEncoder
    implicit val decodesJs: Decoder[JsConfig] = deriveDecoder
    implicit val encodesJs: Encoder[JsConfig] = deriveEncoder
    implicit val decodesNative: Decoder[NativeConfig] = deriveDecoder
    implicit val encodesNative: Encoder[NativeConfig] = deriveEncoder
  }
  case class JvmConfig(
      //      home: Option[Path],
      options: List[String]
  ) extends PlatformConfig

  sealed abstract class LinkerMode(val id: String)

  object LinkerMode extends EnumCodec[LinkerMode] {
    case object Debug extends LinkerMode("debug")
    case object Release extends LinkerMode("release")
    val All = List(Debug, Release).map(x => x.id -> x).toMap
  }

  sealed abstract class ModuleKindJS(val id: String)

  object ModuleKindJS extends EnumCodec[ModuleKindJS] {
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
    implicit val encodes: Encoder[NativeOptions] = deriveEncoder
    implicit val decodes: Decoder[NativeOptions] = deriveDecoder
  }

  case class ProjectName(value: String) extends AnyVal

  object ProjectName {
    implicit val ordering: Ordering[ProjectName] = Ordering.by(_.value)
    implicit val decodes: Decoder[ProjectName] = Decoder[String].map(ProjectName.apply)
    implicit val encodes: Encoder[ProjectName] = Encoder[String].contramap(_.value)
    implicit val keyDecodes: KeyDecoder[ProjectName] = KeyDecoder[String].map(ProjectName.apply)
    implicit val keyEncodes: KeyEncoder[ProjectName] = KeyEncoder[String].contramap(_.value)
  }

  case class Project(
      folder: Option[RelPath],
      dependsOn: Option[JsonList[ProjectName]],
      `source-layout`: Option[SourceLayout],
      `sbt-scope`: Option[String],
      sources: Option[JsonList[RelPath]],
      resources: Option[JsonList[RelPath]],
      dependencies: Option[JsonList[JavaOrScalaDependency]],
      java: Option[Java],
      scala: Option[Scala],
      platform: Option[Platform]
  )

  object Project {
    implicit val decodes: Decoder[Project] = deriveDecoder
    implicit val encodes: Encoder[Project] = deriveEncoder
  }

  case class ScriptName(value: String) extends AnyVal

  object ScriptName {
    implicit val decodes: Decoder[ScriptName] = Decoder[String].map(ScriptName.apply)
    implicit val encodes: Encoder[ScriptName] = Encoder[String].contramap(_.value)
    implicit val keyDecodes: KeyDecoder[ScriptName] = KeyDecoder[String].map(ScriptName.apply)
    implicit val keyEncodes: KeyEncoder[ScriptName] = KeyEncoder[String].contramap(_.value)
  }
  case class ScriptDef(project: ProjectName, main: String)

  object ScriptDef {
    implicit val decodes: Decoder[ScriptDef] = Decoder.instance(c =>
      c.as[String].flatMap { str =>
        str.split("/") match {
          case Array(projectName, main) =>
            Right(ScriptDef(ProjectName(projectName), main))
          case _ =>
            Left(DecodingFailure(s"$str needs to be on the form projectName/fully.qualified.Main", c.history))
        }
      }
    )
    implicit val encodes: Encoder[ScriptDef] = Encoder.instance(sd => Json.fromString(s"${sd.project.value}/${sd.main}"))
  }

  case class Build(
      version: String,
      scala: Option[Scala],
      java: Option[Java],
      scripts: Option[Map[ScriptName, JsonList[ScriptDef]]],
      projects: Map[ProjectName, Project],
      resolvers: Option[JsonList[URI]]
  ) {
    def transitiveDependenciesFor(projName: model.ProjectName): Map[ProjectName, model.Project] = {
      val builder = Map.newBuilder[model.ProjectName, model.Project]

      def go(n: model.ProjectName): Unit = {
        val p = projects.getOrElse(n, sys.error(s"Project ${projName.value} depends on non-existing project ${n.value}"))
        builder += ((n, p))
        p.dependsOn.flat.foreach(go)
      }

      projects(projName).dependsOn.flat.foreach(go)

      builder.result()
    }
  }

  object Build {
    implicit val decodes: Decoder[Build] = deriveDecoder
    implicit val encodes: Encoder[Build] = deriveEncoder
  }

  def parseBuild(json: String): Either[Error, Build] =
    parser.decode[model.Build](json)
}
