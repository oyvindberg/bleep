package bleep

import bleep.internal.codecs.codecURI
import bleep.internal.{EnumCodec, SetLike, ShortenAndSortJson}
import bloop.config.Config
import io.circe._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax._

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

  case class PlatformId(value: String) extends AnyVal
  object PlatformId {
    implicit val ordering: Ordering[PlatformId] = Ordering.by(_.value)
    implicit val decodes: Decoder[PlatformId] = Decoder[String].map(PlatformId.apply)
    implicit val encodes: Encoder[PlatformId] = Encoder[String].contramap(_.value)
    implicit val keyDecodes: KeyDecoder[PlatformId] = KeyDecoder[String].map(PlatformId.apply)
    implicit val keyEncodes: KeyEncoder[PlatformId] = KeyEncoder[String].contramap(_.value)
  }

  case class TemplateId(value: String) extends AnyVal
  object TemplateId {
    implicit val ordering: Ordering[TemplateId] = Ordering.by(_.value)
    implicit val decodes: Decoder[TemplateId] = Decoder[String].map(TemplateId.apply)
    implicit val encodes: Encoder[TemplateId] = Encoder[String].contramap(_.value)
    implicit val keyDecodes: KeyDecoder[TemplateId] = KeyDecoder[String].map(TemplateId.apply)
    implicit val keyEncodes: KeyEncoder[TemplateId] = KeyEncoder[String].contramap(_.value)
  }

  sealed abstract class Platform(val name: PlatformId) extends SetLike[Platform] {
    def compilerPlugin: Option[Dep]

    override def removeAll(other: Platform): Platform =
      safeRemoveAll(other).getOrElse {
        sys.error(s"Cannot mix ${name.value} and ${other.name.value}")
      }

    def safeRemoveAll(other: Platform): Option[Platform] =
      (this, other) match {
        case (one: Platform.Js, two: Platform.Js)         => Some(one.removeAllJs(two))
        case (one: Platform.Jvm, two: Platform.Jvm)       => Some(one.removeAllJvm(two))
        case (one: Platform.Native, two: Platform.Native) => Some(one.removeAllNative(two))
        case _                                            => None
      }

    override def union(other: Platform): Platform =
      (this, other) match {
        case (one: Platform.Js, two: Platform.Js)         => one.unionJs(two)
        case (one: Platform.Jvm, two: Platform.Jvm)       => one.unionJvm(two)
        case (one: Platform.Native, two: Platform.Native) => one.unionNative(two)
        case (one, two)                                   => sys.error(s"Cannot mix ${one.name.value} and ${two.name.value}")
      }

    override def intersect(other: Platform): Platform =
      safeIntersect(other).getOrElse {
        sys.error(s"Cannot mix ${name.value} and ${other.name.value}")
      }

    def safeIntersect(other: Platform): Option[Platform] =
      (this, other) match {
        case (one: Platform.Js, two: Platform.Js)         => Some(one.intersectJs(two))
        case (one: Platform.Jvm, two: Platform.Jvm)       => Some(one.intersectJvm(two))
        case (one: Platform.Native, two: Platform.Native) => Some(one.intersectNative(two))
        case _                                            => None
      }

    override def isEmpty: Boolean =
      this match {
        case Platform.Js(version, mode, kind, emitSourceMaps, jsdom, mainClass) =>
          version.isEmpty && mode.isEmpty && kind.isEmpty & emitSourceMaps.isEmpty & jsdom.isEmpty & mainClass.isEmpty
        case Platform.Jvm(options, mainClass, runtimeOptions) =>
          options.isEmpty && mainClass.isEmpty && runtimeOptions.isEmpty
        case Platform.Native(version, mode, gc, mainClass) =>
          version.isEmpty && mode.isEmpty && gc.isEmpty && mainClass.isEmpty
      }
  }

  object Platform {
    case class Js(
        version: Option[Versions.ScalaJs],
        mode: Option[Config.LinkerMode],
        kind: Option[Config.ModuleKindJS],
        emitSourceMaps: Option[Boolean],
        jsdom: Option[Boolean],
//      output: Option[Path],
//      nodePath: Option[Path],
//      toolchain: List[Path]
        mainClass: Option[String]
//        mapSourceURI: Option[URI]
    ) extends Platform(PlatformId("js")) {

      override def compilerPlugin: Option[Dep] = version.map(_.compilerPlugin)

      def intersectJs(other: Js): Js =
        Js(
          version = if (version == other.version) version else None,
          mode = if (mode == other.mode) mode else None,
          kind = if (kind == other.kind) kind else None,
          emitSourceMaps = if (emitSourceMaps == other.emitSourceMaps) emitSourceMaps else None,
          jsdom = if (jsdom == other.jsdom) jsdom else None,
          mainClass = if (mainClass == other.mainClass) mainClass else None
//          mapSourceURI = if (mapSourceURI == other.mapSourceURI) mapSourceURI else None
        )

      def removeAllJs(other: Js): Js =
        Js(
          version = if (version == other.version) None else version,
          mode = if (mode == other.mode) None else mode,
          kind = if (kind == other.kind) None else kind,
          emitSourceMaps = if (emitSourceMaps == other.emitSourceMaps) None else emitSourceMaps,
          jsdom = if (jsdom == other.jsdom) None else jsdom,
          mainClass = if (mainClass == other.mainClass) None else mainClass
//          mapSourceURI = if (mapSourceURI == other.mapSourceURI) None else mapSourceURI
        )

      def unionJs(other: Js): Js =
        Js(
          version = version.orElse(other.version),
          mode = mode.orElse(other.mode),
          kind = kind.orElse(other.kind),
          emitSourceMaps = emitSourceMaps.orElse(other.emitSourceMaps),
          jsdom = jsdom.orElse(other.jsdom),
          mainClass = mainClass.orElse(other.mainClass)
//          mapSourceURI = mapSourceURI.orElse(other.mapSourceURI)
        )
    }

    case class Jvm(
        //      home: Option[Path],
        options: Options,
        mainClass: Option[String],
        //      runtimeHome: Option[Path],
        runtimeOptions: Options
        //        classpath: Option[List[Path]],
        //        resources: Option[List[Path]]
    ) extends Platform(PlatformId("jvm")) {

      override def compilerPlugin: Option[Dep] = None

      def intersectJvm(other: Jvm): Jvm =
        Jvm(
          options = options.intersect(other.options),
          mainClass = if (mainClass == other.mainClass) mainClass else None,
          runtimeOptions = runtimeOptions.intersect(other.runtimeOptions)
        )

      def removeAllJvm(other: Jvm): Jvm =
        Jvm(
          options = options.removeAll(other.options),
          mainClass = if (mainClass == other.mainClass) None else mainClass,
          runtimeOptions = runtimeOptions.removeAll(other.runtimeOptions)
        )

      def unionJvm(other: Jvm): Jvm =
        Jvm(
          options = options.union(other.options),
          mainClass = mainClass.orElse(other.mainClass),
          runtimeOptions = runtimeOptions.union(other.runtimeOptions)
        )
    }

    case class Native(
        version: Option[Versions.ScalaNative],
        mode: Option[Config.LinkerMode],
        gc: Option[String],
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
        mainClass: Option[String]
    ) extends Platform(PlatformId("native")) {

      override def compilerPlugin: Option[Dep] = ???

      def intersectNative(other: Native): Native =
        Native(
          version = if (version == other.version) version else None,
          mode = if (mode == other.mode) mode else None,
          gc = if (gc == other.gc) gc else None,
          mainClass = if (mainClass == other.mainClass) mainClass else None
        )

      def removeAllNative(other: Native): Native =
        Native(
          version = if (version == other.version) None else version,
          mode = if (mode == other.mode) None else mode,
          gc = if (gc == other.gc) None else gc,
          mainClass = if (mainClass == other.mainClass) None else mainClass
        )

      def unionNative(other: Native): Native =
        Native(
          version = version.orElse(other.version),
          mode = mode.orElse(other.mode),
          gc = gc.orElse(other.gc),
          mainClass = mainClass.orElse(other.mainClass)
        )
    }

    implicit val decodesScalaJsVersion: Decoder[Versions.ScalaJs] = Decoder[String].map(Versions.ScalaJs.apply)
    implicit val encodesScalaJsVersion: Encoder[Versions.ScalaJs] = Encoder[String].contramap(_.scalaJsVersion)
    implicit val decodesScalaNativeVersion: Decoder[Versions.ScalaNative] = Decoder[String].map(Versions.ScalaNative.apply)
    implicit val encodesScalaNativeVersion: Encoder[Versions.ScalaNative] = Encoder[String].contramap(_.scalaNativeVersion)

    val decodesJs: Decoder[Js] = deriveDecoder
    val encodesJs: Encoder[Js] = deriveEncoder
    val decodesJvm: Decoder[Jvm] = deriveDecoder
    val encodesJvm: Encoder[Jvm] = deriveEncoder
    val decodesNative: Decoder[Native] = deriveDecoder
    val encodesNative: Encoder[Native] = deriveEncoder

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

    implicit val encodes: Encoder[Platform] = Encoder.instance { p =>
      val json = p match {
        case x: Js     => encodesJs(x)
        case x: Jvm    => encodesJvm(x)
        case x: Native => encodesNative(x)
      }

      json.foldWith(ShortenAndSortJson).mapObject(obj => obj.add("name", p.name.asJson))
    }
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

  case class Project(
      `extends`: JsonList[TemplateId],
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
        folder = if (folder == other.folder) folder else None,
        dependsOn = dependsOn.intersect(other.dependsOn),
        `source-layout` = if (`source-layout` == other.`source-layout`) `source-layout` else None,
        `sbt-scope` = if (`sbt-scope` == other.`sbt-scope`) `sbt-scope` else None,
        sources = sources.intersect(other.sources),
        resources = resources.intersect(other.resources),
        dependencies = dependencies.intersect(other.dependencies),
        java = java.zip(other.java).map { case (_1, _2) => _1.intersect(_2) },
        scala = scala.zip(other.scala).map { case (_1, _2) => _1.intersect(_2) },
        platform = platform.zip(other.platform).flatMap { case (_1, _2) => _1.safeIntersect(_2) },
        testFrameworks = testFrameworks.intersect(other.testFrameworks)
      )

    override def removeAll(other: Project): Project =
      Project(
        `extends` = `extends`.removeAll(other.`extends`),
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
          case (Some(one), Some(two)) => one.safeRemoveAll(two).orElse(platform)
          case _                      => platform
        },
        testFrameworks = testFrameworks.removeAll(other.testFrameworks)
      )

    override def union(other: Project): Project =
      Project(
        `extends` = `extends`.union(other.`extends`),
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
      case Project(extends_, folder, dependsOn, sourceLayout, sbtScope, sources, resources, dependencies, java, scala, platform, testFrameworks) =>
        extends_.isEmpty && folder.isEmpty && dependsOn.isEmpty && sourceLayout.isEmpty && sbtScope.isEmpty && sources.isEmpty && resources.isEmpty && dependencies.isEmpty && java
          .fold(true)(_.isEmpty) && scala.fold(true)(_.isEmpty) && platform.fold(true)(_.isEmpty) && testFrameworks.isEmpty
    }
  }

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
    implicit val encodes: Encoder[ScriptDef] =
      Encoder.instance(sd => Json.fromString(s"${sd.project.value}/${sd.main}"))
  }

  case class Build(
      version: String,
      templates: Option[Map[TemplateId, Project]],
      scripts: Option[Map[ScriptName, JsonList[ScriptDef]]],
      resolvers: JsonSet[URI],
      projects: Map[ProjectName, Project]
  ) {
    def explode(project: Project): Project = {
      def go(project: Project): Project =
        project.`extends`.values.foldLeft(project) { case (p, parentId) =>
          p.union(go(templates.get(parentId)))
        }

      go(project)
    }

    def transitiveDependenciesFor(projName: model.ProjectName): Map[ProjectName, model.Project] = {
      val builder = Map.newBuilder[model.ProjectName, model.Project]

      def go(n: model.ProjectName): Unit = {
        val p = projects.getOrElse(n, sys.error(s"Project ${projName.value} depends on non-existing project ${n.value}"))
        builder += ((n, p))
        p.dependsOn.values.foreach(go)
      }

      projects(projName).dependsOn.values.foreach(go)

      builder.result()
    }
  }

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
