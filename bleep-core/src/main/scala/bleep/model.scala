package bleep

import bleep.internal.EnumCodec
import bloop.config.Config
import coursier.core.Configuration
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe._
import coursier.parse.{DependencyParser, JavaOrScalaDependency}

import java.net.URI

object model {

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
  ) {
    def isEmpty: Boolean =
      this match {
        case CompileSetup(None, None, None, None, None, None) => true
        case _                                                => false
      }

    def intersect(other: CompileSetup): CompileSetup =
      CompileSetup(
        order = if (order == other.order) order else None,
        addLibraryToBootClasspath = if (addLibraryToBootClasspath == other.addLibraryToBootClasspath) addLibraryToBootClasspath else None,
        addCompilerToClasspath = if (addCompilerToClasspath == other.addCompilerToClasspath) addCompilerToClasspath else None,
        addExtraJarsToClasspath = if (addExtraJarsToClasspath == other.addExtraJarsToClasspath) addExtraJarsToClasspath else None,
        manageBootClasspath = if (manageBootClasspath == other.manageBootClasspath) manageBootClasspath else None,
        filterLibraryFromClasspath = if (filterLibraryFromClasspath == other.filterLibraryFromClasspath) filterLibraryFromClasspath else None
      )

    def removeAll(other: CompileSetup): CompileSetup =
      CompileSetup(
        order = if (order == other.order) None else order,
        addLibraryToBootClasspath = if (addLibraryToBootClasspath == other.addLibraryToBootClasspath) None else addLibraryToBootClasspath,
        addCompilerToClasspath = if (addCompilerToClasspath == other.addCompilerToClasspath) None else addCompilerToClasspath,
        addExtraJarsToClasspath = if (addExtraJarsToClasspath == other.addExtraJarsToClasspath) None else addExtraJarsToClasspath,
        manageBootClasspath = if (manageBootClasspath == other.manageBootClasspath) None else manageBootClasspath,
        filterLibraryFromClasspath = if (filterLibraryFromClasspath == other.filterLibraryFromClasspath) None else filterLibraryFromClasspath
      )

    def or(other: CompileSetup): CompileSetup =
      CompileSetup(
        order = order.orElse(other.order),
        addLibraryToBootClasspath = addLibraryToBootClasspath.orElse(other.addLibraryToBootClasspath),
        addCompilerToClasspath = addCompilerToClasspath.orElse(other.addCompilerToClasspath),
        addExtraJarsToClasspath = addExtraJarsToClasspath.orElse(other.addExtraJarsToClasspath),
        manageBootClasspath = manageBootClasspath.orElse(other.manageBootClasspath),
        filterLibraryFromClasspath = filterLibraryFromClasspath.orElse(other.filterLibraryFromClasspath)
      )
  }

  object CompileSetup {
    implicit val decodes: Decoder[CompileSetup] = deriveDecoder
    implicit val encodes: Encoder[CompileSetup] = deriveEncoder
  }

  case class ScalaId(value: String) extends AnyVal
  object ScalaId {
    implicit val ordering: Ordering[ScalaId] = Ordering.by(_.value)
    implicit val decodes: Decoder[ScalaId] = Decoder[String].map(ScalaId.apply)
    implicit val encodes: Encoder[ScalaId] = Encoder[String].contramap(_.value)
    implicit val keyDecodes: KeyDecoder[ScalaId] = KeyDecoder[String].map(ScalaId.apply)
    implicit val keyEncodes: KeyEncoder[ScalaId] = KeyEncoder[String].contramap(_.value)
  }

  case class Scala(
      `extends`: Option[ScalaId],
      version: Option[Versions.Scala],
      options: Option[Options],
      setup: Option[CompileSetup]
  ) {
    def intersect(other: Scala): Scala =
      Scala(
        `extends` = if (`extends` == other.`extends`) `extends` else None,
        version = if (`version` == other.`version`) `version` else None,
        options = options.zip(other.options).map { case (_1, _2) => _1.intersect(_2) }.filterNot(_.isEmpty),
        setup = setup.zip(other.setup).map { case (_1, _2) => _1.intersect(_2) }.filterNot(_.isEmpty)
      )

    def removeAll(other: Scala): Scala =
      Scala(
        `extends` = if (`extends` == other.`extends`) None else `extends`,
        version = if (`version` == other.`version`) None else `version`,
        options = options.zip(other.options).map { case (_1, _2) => _1.removeAll(_2) }.filterNot(_.isEmpty),
        setup = setup.zip(other.setup).map { case (_1, _2) => _1.removeAll(_2) }.filterNot(_.isEmpty)
      )

    def or(other: Scala): Scala =
      Scala(
        `extends` = `extends`.orElse(other.`extends`),
        version = version.orElse(other.version),
        options = List(options, other.options).flatten.reduceOption(_ ++ _),
        setup = List(setup, other.setup).flatten.reduceOption(_ or _)
      )
  }

  object Scala {
    implicit val decodesScala: Decoder[Versions.Scala] = Decoder[String].map(Versions.Scala.apply)
    implicit val encodesScala: Encoder[Versions.Scala] = Encoder[String].contramap(_.scalaVersion)

    val fullDecodes: Decoder[Scala] = deriveDecoder
    val fullEncodes: Encoder[Scala] = deriveEncoder

    implicit val decodes: Decoder[Scala] = Decoder.instance(c =>
      c.as[Option[ScalaId]].flatMap {
        case Some(extends_) => Right(Scala(`extends` = Some(extends_), None, None, None))
        case None           => fullDecodes(c)
      }
    )

    implicit val encodes: Encoder[Scala] = Encoder.instance {
      case Scala(Some(extends_), None, None, None) => Encoder[ScalaId].apply(extends_)
      case full                                    => fullEncodes(full)
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
    implicit val decodesDep: Decoder[JavaOrScalaDependency] =
      Decoder.instance(c =>
        for {
          str <- c.as[String]
          tuple <- DependencyParser.javaOrScalaDependencyParams(str, Configuration.empty).left.map(err => DecodingFailure(err, c.history))
        } yield tuple._1
      )

    implicit val encodesDep: Encoder[JavaOrScalaDependency] =
      Encoder.instance {
        case dep: JavaOrScalaDependency.JavaDependency =>
          Json.fromString(s"${dep.module}:${dep.dependency.version}")
        case dep: JavaOrScalaDependency.ScalaDependency => Json.fromString(dep.repr)
      }

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
      projects: Map[ProjectName, Project],
      scala: Option[Map[ScalaId, Scala]],
      java: Option[Java],
      scripts: Option[Map[ScriptName, JsonList[ScriptDef]]],
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
    implicit val decodesURI: Decoder[URI] = Decoder[String].emap(str =>
      try Right(URI.create(str))
      catch { case x: IllegalArgumentException => Left(x.getMessage) }
    )
    implicit val encodesURI: Encoder[URI] = Encoder[String].contramap(_.toString)

    implicit val decodes: Decoder[Build] = deriveDecoder
    implicit val encodes: Encoder[Build] = deriveEncoder
  }

  def parseBuild(json: String): Either[Error, Build] =
    parser.decode[model.Build](json)
}
