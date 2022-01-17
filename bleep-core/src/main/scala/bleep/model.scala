package bleep

import bleep.internal.codecs.codecURI
import bleep.internal.{EnumCodec, SetLike}
import bloop.config.Config
import io.circe._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

import java.net.URI

object model {
  case class Java(options: Options) extends SetLike[Java] {
    override def intersect(other: Java): Java =
      Java(options = options.intersect(other.options))

    override def removeAll(other: Java): Java =
      Java(options = options.removeAll(other.options))

    override def union(other: Java): Java =
      Java(options = options.union(other.options))

    def isEmpty: Boolean =
      this match {
        case Java(options) => options.isEmpty
      }
  }

  object Java {
    implicit val decodes: Decoder[Java] = deriveDecoder
    implicit val encodes: Encoder[Java] = deriveEncoder
  }
  case class TestFrameworkName(value: String)

  object TestFrameworkName {
    implicit val decodes: Decoder[TestFrameworkName] = Decoder[String].map(TestFrameworkName.apply)
    implicit val encodes: Encoder[TestFrameworkName] = Encoder[String].contramap(_.value)
    implicit val ordering: Ordering[TestFrameworkName] = Ordering[String].on(_.value)
  }

  implicit val compileOrder: Codec[Config.CompileOrder] =
    EnumCodec.codec(List(Config.Mixed, Config.JavaThenScala, Config.ScalaThenJava).map(x => x.id -> x).toMap)

  case class CompileSetup(
      order: Option[Config.CompileOrder],
      addLibraryToBootClasspath: Option[Boolean],
      addCompilerToClasspath: Option[Boolean],
      addExtraJarsToClasspath: Option[Boolean],
      manageBootClasspath: Option[Boolean],
      filterLibraryFromClasspath: Option[Boolean]
  ) extends SetLike[CompileSetup] {

    override def intersect(other: CompileSetup): CompileSetup =
      CompileSetup(
        order = if (order == other.order) order else None,
        addLibraryToBootClasspath = if (addLibraryToBootClasspath == other.addLibraryToBootClasspath) addLibraryToBootClasspath else None,
        addCompilerToClasspath = if (addCompilerToClasspath == other.addCompilerToClasspath) addCompilerToClasspath else None,
        addExtraJarsToClasspath = if (addExtraJarsToClasspath == other.addExtraJarsToClasspath) addExtraJarsToClasspath else None,
        manageBootClasspath = if (manageBootClasspath == other.manageBootClasspath) manageBootClasspath else None,
        filterLibraryFromClasspath = if (filterLibraryFromClasspath == other.filterLibraryFromClasspath) filterLibraryFromClasspath else None
      )

    override def removeAll(other: CompileSetup): CompileSetup =
      CompileSetup(
        order = if (order == other.order) None else order,
        addLibraryToBootClasspath = if (addLibraryToBootClasspath == other.addLibraryToBootClasspath) None else addLibraryToBootClasspath,
        addCompilerToClasspath = if (addCompilerToClasspath == other.addCompilerToClasspath) None else addCompilerToClasspath,
        addExtraJarsToClasspath = if (addExtraJarsToClasspath == other.addExtraJarsToClasspath) None else addExtraJarsToClasspath,
        manageBootClasspath = if (manageBootClasspath == other.manageBootClasspath) None else manageBootClasspath,
        filterLibraryFromClasspath = if (filterLibraryFromClasspath == other.filterLibraryFromClasspath) None else filterLibraryFromClasspath
      )

    override def union(other: CompileSetup): CompileSetup =
      CompileSetup(
        order = order.orElse(other.order),
        addLibraryToBootClasspath = addLibraryToBootClasspath.orElse(other.addLibraryToBootClasspath),
        addCompilerToClasspath = addCompilerToClasspath.orElse(other.addCompilerToClasspath),
        addExtraJarsToClasspath = addExtraJarsToClasspath.orElse(other.addExtraJarsToClasspath),
        manageBootClasspath = manageBootClasspath.orElse(other.manageBootClasspath),
        filterLibraryFromClasspath = filterLibraryFromClasspath.orElse(other.filterLibraryFromClasspath)
      )

    override def isEmpty: Boolean = this match {
      case CompileSetup(order, addLibraryToBootClasspath, addCompilerToClasspath, addExtraJarsToClasspath, manageBootClasspath, filterLibraryFromClasspath) =>
        order.isEmpty && addLibraryToBootClasspath.isEmpty && addCompilerToClasspath.isEmpty && addExtraJarsToClasspath.isEmpty && manageBootClasspath.isEmpty && filterLibraryFromClasspath.isEmpty
    }
  }

  object CompileSetup {
    implicit val decodes: Decoder[CompileSetup] = deriveDecoder
    implicit val encodes: Encoder[CompileSetup] = deriveEncoder
  }

  case class Scala(
      version: Option[Versions.Scala],
      options: Options,
      setup: Option[CompileSetup],
      compilerPlugins: JsonSet[Dep]
  ) extends SetLike[Scala] {
    override def intersect(other: Scala): Scala =
      Scala(
        version = if (`version` == other.`version`) `version` else None,
        options = options.intersect(other.options),
        setup = setup.zip(other.setup).map { case (_1, _2) => _1.intersect(_2) },
        compilerPlugins = compilerPlugins.intersect(other.compilerPlugins)
      )

    override def removeAll(other: Scala): Scala =
      Scala(
        version = if (`version` == other.`version`) None else `version`,
        options = options.removeAll(other.options),
        setup = List(setup, other.setup).flatten.reduceOption(_ removeAll _),
        compilerPlugins = compilerPlugins.removeAll(other.compilerPlugins)
      )

    override def union(other: Scala): Scala =
      Scala(
        version = version.orElse(other.version),
        options = options.union(other.options),
        setup = List(setup, other.setup).flatten.reduceOption(_ union _),
        compilerPlugins = compilerPlugins.union(other.compilerPlugins)
      )

    override def isEmpty: Boolean =
      this match {
        case Scala(version, options, setup, compilerPlugins) =>
          version.isEmpty && options.isEmpty && setup.fold(true)(_.isEmpty) && compilerPlugins.isEmpty
      }
  }

  object Scala {
    implicit val decodesScala: Decoder[Versions.Scala] = Decoder[String].map(Versions.Scala.apply)
    implicit val encodesScala: Encoder[Versions.Scala] = Encoder[String].contramap(_.scalaVersion)

    implicit val decodes: Decoder[Scala] = deriveDecoder
    implicit val Encodes: Encoder[Scala] = deriveEncoder
  }

  sealed abstract class PlatformId(val value: String)
  object PlatformId {
    case object Jvm extends PlatformId("jvm")
    case object Js extends PlatformId("js")
    case object Native extends PlatformId("native")
    val All = List[PlatformId](Jvm, Js, Native)
    def fromName(str: String): Option[PlatformId] = All.find(_.value == str)

    implicit val ordering: Ordering[PlatformId] = Ordering.by(All.indexOf)
    implicit val decodes: Decoder[PlatformId] =
      Decoder[String].emap(str => fromName(str).toRight(s"${str} is not among ${All.map(_.value).mkString(", ")}"))
    implicit val encodes: Encoder[PlatformId] = Encoder[String].contramap(_.value)
  }

  case class TemplateId(value: String) extends AnyVal
  object TemplateId {
    implicit val ordering: Ordering[TemplateId] = Ordering.by(_.value)
    implicit val decodes: Decoder[TemplateId] = Decoder[String].map(TemplateId.apply)
    implicit val encodes: Encoder[TemplateId] = Encoder[String].contramap(_.value)
    implicit val keyDecodes: KeyDecoder[TemplateId] = KeyDecoder[String].map(TemplateId.apply)
    implicit val keyEncodes: KeyEncoder[TemplateId] = KeyEncoder[String].contramap(_.value)
  }

  case class Platform(
      name: Option[PlatformId],
      jsVersion: Option[Versions.ScalaJs],
      jsMode: Option[Config.LinkerMode],
      jsKind: Option[Config.ModuleKindJS],
      jsEmitSourceMaps: Option[Boolean],
      jsJsdom: Option[Boolean],
      //      output: Option[Path],
      //      nodePath: Option[Path],
      //      toolchain: List[Path]
      jsMainClass: Option[String],
      //        mapSourceURI: Option[URI]
      //      home: Option[Path],
      jvmOptions: Options,
      jvmMainClass: Option[String],
      //      runtimeHome: Option[Path],
      jvmRuntimeOptions: Options,
      //        classpath: Option[List[Path]],
      //        resources: Option[List[Path]]
      nativeVersion: Option[Versions.ScalaNative],
      nativeMode: Option[Config.LinkerMode],
      nativeGc: Option[String],
      //      targetTriple: Option[String],
      //      clang: Path,
      //      clangpp: Path,
      //      toolchain: List[Path],
      //      linker: Option[List[String]],
      //      compiler: Option[List[String]],
      //      linkStubs: Option[Boolean],
      //      check: Option[Boolean],
      //      dump: Option[Boolean],
      //      output: Option[Path]
      nativeMainClass: Option[String]
  ) extends SetLike[Platform] {

    def compilerPlugin: Option[Dep] =
      this match {
        case model.Platform.Js(platform)     => platform.jsVersion.map(_.compilerPlugin)
        case model.Platform.Native(platform) => platform.nativeVersion.map(_.compilerPlugin)
        case _                               => None
      }

    override def intersect(other: Platform): Platform =
      new Platform(
        name = if (name == other.name) name else None,
        jsVersion = if (jsVersion == other.jsVersion) jsVersion else None,
        jsMode = if (jsMode == other.jsMode) jsMode else None,
        jsKind = if (jsKind == other.jsKind) jsKind else None,
        jsEmitSourceMaps = if (jsEmitSourceMaps == other.jsEmitSourceMaps) jsEmitSourceMaps else None,
        jsJsdom = if (jsJsdom == other.jsJsdom) jsJsdom else None,
        jsMainClass = if (jsMainClass == other.jsMainClass) jsMainClass else None,
        //          mapSourceURI = if (mapSourceURI == other.mapSourceURI) mapSourceURI else None
        jvmOptions = jvmOptions.intersect(other.jvmOptions),
        jvmMainClass = if (jvmMainClass == other.jvmMainClass) jvmMainClass else None,
        jvmRuntimeOptions = jvmRuntimeOptions.intersect(other.jvmRuntimeOptions),
        nativeVersion = if (nativeVersion == other.nativeVersion) nativeVersion else None,
        nativeMode = if (nativeMode == other.nativeMode) nativeMode else None,
        nativeGc = if (nativeGc == other.nativeGc) nativeGc else None,
        nativeMainClass = if (nativeMainClass == other.nativeMainClass) nativeMainClass else None
      )

    override def removeAll(other: Platform): Platform =
      new Platform(
        name = if (name == other.name) None else name,
        jsVersion = if (jsVersion == other.jsVersion) None else jsVersion,
        jsMode = if (jsMode == other.jsMode) None else jsMode,
        jsKind = if (jsKind == other.jsKind) None else jsKind,
        jsEmitSourceMaps = if (jsEmitSourceMaps == other.jsEmitSourceMaps) None else jsEmitSourceMaps,
        jsJsdom = if (jsJsdom == other.jsJsdom) None else jsJsdom,
        jsMainClass = if (jsMainClass == other.jsMainClass) None else jsMainClass,
        //          mapSourceURI = if (mapSourceURI == other.mapSourceURI) None else mapSourceURI
        jvmOptions = jvmOptions.removeAll(other.jvmOptions),
        jvmMainClass = if (jvmMainClass == other.jvmMainClass) None else jvmMainClass,
        jvmRuntimeOptions = jvmRuntimeOptions.removeAll(other.jvmRuntimeOptions),
        nativeVersion = if (nativeVersion == other.nativeVersion) None else nativeVersion,
        nativeMode = if (nativeMode == other.nativeMode) None else nativeMode,
        nativeGc = if (nativeGc == other.nativeGc) None else nativeGc,
        nativeMainClass = if (nativeMainClass == other.nativeMainClass) None else nativeMainClass
      )

    override def union(other: Platform): Platform =
      new Platform(
        name = name.orElse(other.name),
        jsVersion = jsVersion.orElse(other.jsVersion),
        jsMode = jsMode.orElse(other.jsMode),
        jsKind = jsKind.orElse(other.jsKind),
        jsEmitSourceMaps = jsEmitSourceMaps.orElse(other.jsEmitSourceMaps),
        jsJsdom = jsJsdom.orElse(other.jsJsdom),
        jsMainClass = jsMainClass.orElse(other.jsMainClass),
        //          mapSourceURI = mapSourceURI.orElse(other.mapSourceURI)
        jvmOptions = jvmOptions.union(other.jvmOptions),
        jvmMainClass = jvmMainClass.orElse(other.jvmMainClass),
        jvmRuntimeOptions = jvmRuntimeOptions.union(other.jvmRuntimeOptions),
        nativeVersion = nativeVersion.orElse(other.nativeVersion),
        nativeMode = nativeMode.orElse(other.nativeMode),
        nativeGc = nativeGc.orElse(other.nativeGc),
        nativeMainClass = nativeMainClass.orElse(other.nativeMainClass)
      )

    override def isEmpty: Boolean =
      name.isEmpty && jsVersion.isEmpty && jsMode.isEmpty && jsKind.isEmpty & jsEmitSourceMaps.isEmpty & jsJsdom.isEmpty & jsMainClass.isEmpty &&
        jvmOptions.isEmpty && jvmMainClass.isEmpty && jvmRuntimeOptions.isEmpty &&
        nativeVersion.isEmpty && nativeMode.isEmpty && nativeGc.isEmpty && nativeMainClass.isEmpty
  }

  object Platform {
    object Jvm {
      def apply(jvmOptions: Options, jvmMainClass: Option[String], jvmRuntimeOptions: Options) =
        new Platform(
          name = Some(PlatformId.Jvm),
          jsVersion = None,
          jsMode = None,
          jsKind = None,
          jsEmitSourceMaps = None,
          jsJsdom = None,
          jsMainClass = None,
          jvmOptions = jvmOptions,
          jvmMainClass = jvmMainClass,
          jvmRuntimeOptions = jvmRuntimeOptions,
          nativeVersion = None,
          nativeMode = None,
          nativeGc = None,
          nativeMainClass = None
        )
      def unapply(x: Platform): Option[Platform] =
        x.name.flatMap {
          case PlatformId.Jvm => Some(x)
          case _              => None
        }

    }
    object Js {
      def apply(
          jsVersion: Option[Versions.ScalaJs],
          jsMode: Option[Config.LinkerMode],
          jsKind: Option[Config.ModuleKindJS],
          jsEmitSourceMaps: Option[Boolean],
          jsJsdom: Option[Boolean],
          jsMainClass: Option[String]
      ) =
        new Platform(
          name = Some(PlatformId.Js),
          jsVersion = jsVersion,
          jsMode = jsMode,
          jsKind = jsKind,
          jsEmitSourceMaps = jsEmitSourceMaps,
          jsJsdom = jsJsdom,
          jsMainClass = jsMainClass,
          jvmOptions = Options.empty,
          jvmMainClass = None,
          jvmRuntimeOptions = Options.empty,
          nativeVersion = None,
          nativeMode = None,
          nativeGc = None,
          nativeMainClass = None
        )
      def unapply(x: Platform): Option[Platform] =
        x.name.flatMap {
          case PlatformId.Js => Some(x)
          case _             => None
        }
    }
    object Native {
      def apply(nativeVersion: Option[Versions.ScalaNative], nativeMode: Option[Config.LinkerMode], nativeGc: Option[String], nativeMainClass: Option[String]) =
        new Platform(
          name = Some(PlatformId.Native),
          jsVersion = None,
          jsMode = None,
          jsKind = None,
          jsEmitSourceMaps = None,
          jsJsdom = None,
          jsMainClass = None,
          jvmOptions = Options.empty,
          jvmMainClass = None,
          jvmRuntimeOptions = Options.empty,
          nativeVersion = nativeVersion,
          nativeMode = nativeMode,
          nativeGc = nativeGc,
          nativeMainClass = nativeMainClass
        )
      def unapply(x: Platform): Option[Platform] =
        x.name.flatMap {
          case PlatformId.Native => Some(x)
          case _                 => None
        }
    }

    implicit val decodesScalaJsVersion: Decoder[Versions.ScalaJs] = Decoder[String].map(Versions.ScalaJs.apply)
    implicit val encodesScalaJsVersion: Encoder[Versions.ScalaJs] = Encoder[String].contramap(_.scalaJsVersion)
    implicit val decodesScalaNativeVersion: Decoder[Versions.ScalaNative] = Decoder[String].map(Versions.ScalaNative.apply)
    implicit val encodesScalaNativeVersion: Encoder[Versions.ScalaNative] = Encoder[String].contramap(_.scalaNativeVersion)
    implicit val decodes: Decoder[Platform] = deriveDecoder
    implicit val encodes: Encoder[Platform] = deriveEncoder
  }

  implicit val linkerModeCodec: Codec[Config.LinkerMode] =
    EnumCodec.codec(List(Config.LinkerMode.Debug, Config.LinkerMode.Release).map(x => x.id -> x).toMap)

  implicit val moduleKindJSCodec: Codec[Config.ModuleKindJS] =
    EnumCodec.codec(List(Config.ModuleKindJS.NoModule, Config.ModuleKindJS.CommonJSModule, Config.ModuleKindJS.ESModule).map(x => x.id -> x).toMap)

  case class ProjectName(value: String) extends AnyVal

  object ProjectName {
    implicit val ordering: Ordering[ProjectName] = Ordering.by(_.value)
    implicit val decodes: Decoder[ProjectName] = Decoder[String].map(ProjectName.apply)
    implicit val encodes: Encoder[ProjectName] = Encoder[String].contramap(_.value)
    implicit val keyDecodes: KeyDecoder[ProjectName] = KeyDecoder[String].map(ProjectName.apply)
    implicit val keyEncodes: KeyEncoder[ProjectName] = KeyEncoder[String].contramap(_.value)
  }

  case class CrossId(value: String)
  object CrossId {
    implicit val ordering: Ordering[CrossId] = Ordering.by(_.value)
    implicit val decodes: Decoder[CrossId] = Decoder[String].map(CrossId.apply)
    implicit val encodes: Encoder[CrossId] = Encoder[String].contramap(_.value)
    implicit val keyDecodes: KeyDecoder[CrossId] = KeyDecoder[String].map(CrossId.apply)
    implicit val keyEncodes: KeyEncoder[CrossId] = KeyEncoder[String].contramap(_.value)
  }

  case class CrossProjectName(name: model.ProjectName, crossId: Option[CrossId]) {
    val value: String =
      crossId match {
        case Some(crossId) => s"${name.value}@${crossId.value}"
        case None          => name.value
      }
  }

  object CrossProjectName {
    implicit val ordering: Ordering[CrossProjectName] = Ordering.by(x => (x.name, x.crossId))
    implicit val decodes: Decoder[CrossProjectName] =
      Decoder.instance(c =>
        for {
          str <- c.as[String]
          crossName <- str.split("@") match {
            case Array(name)          => Right(CrossProjectName(ProjectName(name), None))
            case Array(name, crossId) => Right(CrossProjectName(ProjectName(name), Some(CrossId(crossId))))
            case _                    => Left(DecodingFailure(s"more than one '@' encountered in CrossProjectName $str", c.history))
          }
        } yield crossName
      )

    implicit val encodes: Encoder[CrossProjectName] = Encoder[String].contramap(_.value)
  }

  case class Project(
      `extends`: JsonList[TemplateId],
      cross: JsonMap[CrossId, Project],
      folder: Option[RelPath],
      dependsOn: JsonSet[ProjectName],
      `source-layout`: Option[SourceLayout],
      `sbt-scope`: Option[String],
      sources: JsonSet[RelPath],
      resources: JsonSet[RelPath],
      dependencies: JsonSet[Dep],
      java: Option[Java],
      scala: Option[Scala],
      platform: Option[Platform],
      testFrameworks: JsonSet[TestFrameworkName]
  ) extends SetLike[Project] {
    override def intersect(other: Project): Project =
      Project(
        `extends` = `extends`.intersect(other.`extends`),
        cross = cross.intersect(other.cross),
        folder = if (folder == other.folder) folder else None,
        dependsOn = dependsOn.intersect(other.dependsOn),
        `source-layout` = if (`source-layout` == other.`source-layout`) `source-layout` else None,
        `sbt-scope` = if (`sbt-scope` == other.`sbt-scope`) `sbt-scope` else None,
        sources = sources.intersect(other.sources),
        resources = resources.intersect(other.resources),
        dependencies = dependencies.intersect(other.dependencies),
        java = java.zip(other.java).map { case (_1, _2) => _1.intersect(_2) },
        scala = scala.zip(other.scala).map { case (_1, _2) => _1.intersect(_2) },
        platform = platform.zip(other.platform).flatMap { case (_1, _2) => _1.intersectDropEmpty(_2) },
        testFrameworks = testFrameworks.intersect(other.testFrameworks)
      )

    override def removeAll(other: Project): Project =
      Project(
        `extends` = `extends`.removeAll(other.`extends`),
        cross = cross.removeAll(other.cross),
        folder = if (folder == other.folder) None else folder,
        dependsOn = dependsOn.removeAll(other.dependsOn),
        `source-layout` = if (`source-layout` == other.`source-layout`) None else `source-layout`,
        `sbt-scope` = if (`sbt-scope` == other.`sbt-scope`) None else `sbt-scope`,
        sources = sources.removeAll(other.sources),
        resources = resources.removeAll(other.resources),
        dependencies = dependencies.removeAll(other.dependencies),
        java = List(java, other.java).flatten.reduceOption(_ removeAll _),
        scala = List(scala, other.scala).flatten.reduceOption(_ removeAll _),
        platform = (platform, other.platform) match {
          case (Some(one), Some(two)) => one.removeAllDropEmpty(two)
          case _                      => platform
        },
        testFrameworks = testFrameworks.removeAll(other.testFrameworks)
      )

    override def union(other: Project): Project =
      Project(
        `extends` = `extends`.union(other.`extends`),
        cross = cross.union(other.cross),
        folder = folder.orElse(other.folder),
        dependsOn = dependsOn.union(other.dependsOn),
        `source-layout` = `source-layout`.orElse(other.`source-layout`),
        `sbt-scope` = `sbt-scope`.orElse(other.`sbt-scope`),
        sources = sources.union(other.sources),
        resources = resources.union(other.resources),
        dependencies = dependencies.union(other.dependencies),
        java = List(java, other.java).flatten.reduceOption(_ union _),
        scala = List(scala, other.scala).flatten.reduceOption(_ union _),
        // may throw
        platform = List(platform, other.platform).flatten.reduceOption(_ union _),
        testFrameworks = testFrameworks.union(other.testFrameworks)
      )

    override def isEmpty: Boolean = this match {
      case Project(extends_, variants, folder, dependsOn, sourceLayout, sbtScope, sources, resources, dependencies, java, scala, platform, testFrameworks) =>
        extends_.isEmpty && variants.isEmpty && folder.isEmpty && dependsOn.isEmpty && sourceLayout.isEmpty && sbtScope.isEmpty && sources.isEmpty && resources.isEmpty && dependencies.isEmpty && java
          .fold(true)(_.isEmpty) && scala.fold(true)(_.isEmpty) && platform.fold(true)(_.isEmpty) && testFrameworks.isEmpty
    }
  }

  object Project {
    val empty = model.Project(
      `extends` = JsonList.empty,
      cross = JsonMap.empty,
      folder = None,
      dependsOn = JsonSet.empty,
      `source-layout` = None,
      `sbt-scope` = None,
      sources = JsonSet.empty,
      resources = JsonSet.empty,
      dependencies = JsonSet.empty,
      java = None,
      scala = None,
      platform = None,
      testFrameworks = JsonSet.empty
    )

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
  case class ScriptDef(project: CrossProjectName, main: String)

  object ScriptDef {
    implicit val decodes: Decoder[ScriptDef] = Decoder.instance(c =>
      c.as[String].flatMap { str =>
        str.split("/") match {
          case Array(projectName, main) =>
            CrossProjectName.decodes.decodeJson(Json.fromString(projectName)).map(crossProjectName => ScriptDef(crossProjectName, main))

          case _ =>
            Left(DecodingFailure(s"$str needs to be on the form `projectName(@crossId)/fully.qualified.Main`", c.history))
        }
      }
    )
    implicit val encodes: Encoder[ScriptDef] =
      Encoder.instance(sd => Json.fromString(s"${sd.project.value}/${sd.main}"))
  }

  case class Build(
      version: String,
      templates: Option[Map[TemplateId, Project]],
      scripts: Option[Map[ScriptName, JsonList[ScriptDef]]],
      resolvers: JsonSet[URI],
      projects: Map[ProjectName, Project]
  )

  object Build {
    implicit val decodes: Decoder[Build] =
      Decoder.instance(c =>
        for {
          version <- c.downField("version").as[String]
          templates <- c.downField("templates").as[Option[Map[TemplateId, Project]]]
          projects <- c.downField("projects").as[Map[ProjectName, Project]]
          scripts <- c.downField("scripts").as[Option[Map[ScriptName, JsonList[ScriptDef]]]]
          resolvers <- c.downField("resolvers").as[JsonSet[URI]]
        } yield Build(version, templates, scripts, resolvers, projects)
      )
    implicit val encodes: Encoder[Build] = deriveEncoder
  }

  def parseBuild(json: String): Either[Error, Build] =
    parser.decode[model.Build](json.linesIterator.filterNot(_.trim().startsWith("//")).mkString("\n"))
}
