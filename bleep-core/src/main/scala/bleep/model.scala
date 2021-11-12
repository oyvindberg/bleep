package bleep

import bleep.internal.codecs.codecURI
import bleep.internal.{EnumCodec, SetLike, ShortenJson}
import bloop.config.Config
import coursier.core.Configuration
import coursier.parse.{DependencyParser, JavaOrScalaDependency}
import io.circe._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax._

import java.net.URI

object model {
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

  implicit val orderingDep: Ordering[JavaOrScalaDependency] =
    Ordering.by { incompleteDep =>
      val dep = incompleteDep.dependency("2.13.1")
      (dep.module.repr, dep.version, dep.configuration)
    }

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

    def explode(build: model.Build): model.Java =
      build.java.foldLeft(this)(_ union _)

    def parent(build: model.Build): Option[Java] =
      build.java
  }

  object Java {
    implicit val decodes: Decoder[Java] = deriveDecoder
    implicit val encodes: Encoder[Java] = deriveEncoder
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
      options: Options,
      setup: Option[CompileSetup],
      compilerPlugins: JsonSet[JavaOrScalaDependency]
  ) extends SetLike[Scala] {
    override def intersect(other: Scala): Scala =
      Scala(
        `extends` = if (`extends` == other.`extends`) `extends` else None,
        version = if (`version` == other.`version`) `version` else None,
        options = options.intersect(other.options),
        setup = setup.zip(other.setup).map { case (_1, _2) => _1.intersect(_2) },
        compilerPlugins = compilerPlugins.intersect(other.compilerPlugins)
      )

    override def removeAll(other: Scala): Scala =
      Scala(
        `extends` = if (`extends` == other.`extends`) None else `extends`,
        version = if (`version` == other.`version`) None else `version`,
        options = options.removeAll(other.options),
        setup = List(setup, other.setup).flatten.reduceOption(_ removeAll _),
        compilerPlugins = compilerPlugins.removeAll(other.compilerPlugins)
      )

    override def union(other: Scala): Scala =
      Scala(
        `extends` = `extends`.orElse(other.`extends`),
        version = version.orElse(other.version),
        options = options.union(other.options),
        setup = List(setup, other.setup).flatten.reduceOption(_ union _),
        compilerPlugins = compilerPlugins.union(other.compilerPlugins)
      )

    def explode(build: model.Build): model.Scala = {
      def go(s: model.Scala): model.Scala =
        s.`extends` match {
          case Some(id) =>
            val found = build.scala.flatMap(scalas => scalas.get(id)).getOrElse(sys.error(s"referenced non-existing scala definition ${id.value}"))
            s.union(go(found))
          case None =>
            s
        }

      go(this)
    }
    def parent(build: model.Build): Option[Scala] =
      `extends` map { id =>
        build.scala.flatMap(scalas => scalas.get(id)).getOrElse(sys.error(s"referenced non-existing scala definition ${id.value}"))
      }
  }

  object Scala {
    implicit val decodesScala: Decoder[Versions.Scala] = Decoder[String].map(Versions.Scala.apply)
    implicit val encodesScala: Encoder[Versions.Scala] = Encoder[String].contramap(_.scalaVersion)

    val fullDecodes: Decoder[Scala] = deriveDecoder
    val fullEncodes: Encoder[Scala] = deriveEncoder

    val shortDecodes: Decoder[Scala] =
      Decoder[ScalaId].map(extends_ => Scala(`extends` = Some(extends_), None, Options.empty, None, JsonSet.empty))

    implicit val decodes: Decoder[Scala] =
      shortDecodes.or(fullDecodes)

    implicit val encodes: Encoder[Scala] = Encoder.instance { s =>
      fullEncodes(s).foldWith(ShortenJson).withObject {
        case obj if obj.size == 1 =>
          obj("extends") match {
            case Some(extendsValue) => extendsValue
            case _                  => obj.asJson
          }
        case obj => obj.asJson
      }
    }
  }

  case class PlatformId(value: String) extends AnyVal
  object PlatformId {
    implicit val ordering: Ordering[PlatformId] = Ordering.by(_.value)
    implicit val decodes: Decoder[PlatformId] = Decoder[String].map(PlatformId.apply)
    implicit val encodes: Encoder[PlatformId] = Encoder[String].contramap(_.value)
    implicit val keyDecodes: KeyDecoder[PlatformId] = KeyDecoder[String].map(PlatformId.apply)
    implicit val keyEncodes: KeyEncoder[PlatformId] = KeyEncoder[String].contramap(_.value)
  }

  sealed abstract class Platform(val name: PlatformId) extends SetLike[Platform] {
    def `extends`: Option[PlatformId]
    def compilerPlugin: Option[JavaOrScalaDependency]

    def explode(build: model.Build): model.Platform = {
      def go(p: model.Platform): model.Platform =
        p.parent(build) match {
          case Some(foundParent) => p.union(go(foundParent))
          case None              => p
        }

      go(this)
    }

    def parent(build: model.Build): Option[Platform] =
      `extends` map { id =>
        build.platforms
          .flatMap(platforms => platforms.get(id))
          .getOrElse(sys.error(s"Platform ${name.value} referenced non-existing platform definition ${id.value}"))
      }

    override def removeAll(other: Platform): Platform =
      (this, other) match {
        case (one: Platform.Js, two: Platform.Js)         => one.removeAllJs(two)
        case (one: Platform.Jvm, two: Platform.Jvm)       => one.removeAllJvm(two)
        case (one: Platform.Native, two: Platform.Native) => one.removeAllNative(two)
        case (one, two)                                   => sys.error(s"Cannot mix ${one.name.value} and ${two.name.value}")
      }

    override def union(other: Platform): Platform =
      (this, other) match {
        case (one: Platform.Js, two: Platform.Js)         => one.unionJs(two)
        case (one: Platform.Jvm, two: Platform.Jvm)       => one.unionJvm(two)
        case (one: Platform.Native, two: Platform.Native) => one.unionNative(two)
        case (one, two)                                   => sys.error(s"Cannot mix ${one.name.value} and ${two.name.value}")
      }

    override def intersect(other: Platform): Platform =
      (this, other) match {
        case (one: Platform.Js, two: Platform.Js)         => one.intersectJs(two)
        case (one: Platform.Jvm, two: Platform.Jvm)       => one.intersectJvm(two)
        case (one: Platform.Native, two: Platform.Native) => one.intersectNative(two)
        case (one, two)                                   => sys.error(s"Cannot mix ${one.name.value} and ${two.name.value}")
      }
  }

  object Platform {
    case class Js(
        `extends`: Option[PlatformId],
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

      override def compilerPlugin: Option[JavaOrScalaDependency] = version.map(_.compilerPlugin)

      def intersectJs(other: Js): Js =
        Js(
          `extends` = if (`extends` == other.`extends`) `extends` else None,
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
          `extends` = if (`extends` == other.`extends`) None else `extends`,
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
          `extends` = `extends`.orElse(other.`extends`),
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
        `extends`: Option[PlatformId],
        //      home: Option[Path],
        options: Options,
        mainClass: Option[String],
        //      runtimeHome: Option[Path],
        runtimeOptions: Options
        //        classpath: Option[List[Path]],
        //        resources: Option[List[Path]]
    ) extends Platform(PlatformId("jvm")) {

      override def compilerPlugin: Option[JavaOrScalaDependency] = None

      def intersectJvm(other: Jvm): Jvm =
        Jvm(
          `extends` = if (`extends` == other.`extends`) `extends` else None,
          options = options.intersect(other.options),
          mainClass = if (mainClass == other.mainClass) mainClass else None,
          runtimeOptions = runtimeOptions.intersect(other.runtimeOptions)
        )

      def removeAllJvm(other: Jvm): Jvm =
        Jvm(
          `extends` = if (`extends` == other.`extends`) None else `extends`,
          options = options.removeAll(other.options),
          mainClass = if (mainClass == other.mainClass) None else mainClass,
          runtimeOptions = runtimeOptions.removeAll(other.runtimeOptions)
        )

      def unionJvm(other: Jvm): Jvm =
        Jvm(
          `extends` = `extends`.orElse(other.`extends`),
          options = options.union(other.options),
          mainClass = mainClass.orElse(other.mainClass),
          runtimeOptions = runtimeOptions.union(other.runtimeOptions)
        )
    }

    case class Native(
        `extends`: Option[PlatformId],
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

      override def compilerPlugin: Option[JavaOrScalaDependency] = ???

      def intersectNative(other: Native): Native =
        Native(
          `extends` = if (`extends` == other.`extends`) `extends` else None,
          version = if (version == other.version) version else None,
          mode = if (mode == other.mode) mode else None,
          gc = if (gc == other.gc) gc else None,
          mainClass = if (mainClass == other.mainClass) mainClass else None
        )

      def removeAllNative(other: Native): Native =
        Native(
          `extends` = if (`extends` == other.`extends`) None else `extends`,
          version = if (version == other.version) None else version,
          mode = if (mode == other.mode) None else mode,
          gc = if (gc == other.gc) None else gc,
          mainClass = if (mainClass == other.mainClass) None else mainClass
        )

      def unionNative(other: Native): Native =
        Native(
          `extends` = `extends`.orElse(other.`extends`),
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

    val decodeFull: Decoder[Platform] = Decoder.instance(c =>
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

    def decodes(platforms: Option[Map[PlatformId, Platform]]): Decoder[Platform] =
      platforms match {
        case Some(platforms) =>
          val decodeShort = PlatformId.decodes.emap { id =>
            platforms.get(id) match {
              case Some(_: Js)     => Right(Js(`extends` = Some(id), None, None, None, None, None, None))
              case Some(_: Jvm)    => Right(Jvm(`extends` = Some(id), Options.empty, None, Options.empty))
              case Some(_: Native) => Right(Native(`extends` = Some(id), None, None, None, None))
              case None            => Left(s"${id.value} is not a defined platform")
            }
          }

          val hasExtendsButNotName = Decoder.instance(c =>
            c.get[PlatformId]("extends").flatMap { id =>
              platforms.get(id) match {
                case Some(found) =>
                  found match {
                    case _: Js     => decodesJs(c)
                    case _: Jvm    => decodesJvm(c)
                    case _: Native => decodesNative(c)
                  }
                case None => Left(DecodingFailure(s"${id.value} is not a defined platform", c.history))
              }
            }
          )

          decodeShort.or(hasExtendsButNotName).or(decodeFull)

        case None => decodeFull
      }

    implicit val encodes: Encoder[Platform] = Encoder.instance { p =>
      val json = p match {
        case x: Js     => encodesJs(x)
        case x: Jvm    => encodesJvm(x)
        case x: Native => encodesNative(x)
      }

      val shortened = json.foldWith(ShortenJson)
      shortened
        .withObject {
          case obj if obj.size == 1 && obj("extends").nonEmpty =>
            obj("extends").get
          case obj =>
            if (obj("extends").nonEmpty) obj.asJson else obj.add("name", p.name.asJson).asJson
        }
        .withNull(Json.fromFields(List("name" -> p.name.asJson)))
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
      folder: Option[RelPath],
      dependsOn: JsonSet[ProjectName],
      `source-layout`: Option[SourceLayout],
      `sbt-scope`: Option[String],
      sources: JsonSet[RelPath],
      resources: JsonSet[RelPath],
      dependencies: JsonSet[JavaOrScalaDependency],
      java: Option[Java],
      scala: Option[Scala],
      platform: Option[Platform]
  )

  object Project {
    def decodes(implicit platformDecoder: Decoder[Platform]): Decoder[Project] = deriveDecoder
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
      platforms: Option[Map[PlatformId, Platform]],
      scala: Option[Map[ScalaId, Scala]],
      java: Option[Java],
      scripts: Option[Map[ScriptName, JsonList[ScriptDef]]],
      resolvers: JsonSet[URI],
      projects: Map[ProjectName, Project]
  ) {

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
          platforms <- c.downField("platforms").as(Decoder.decodeOption(Decoder.decodeMap(PlatformId.keyDecodes, Platform.decodes(None))))
          projects <- c
            .downField("projects")
            .as[Map[ProjectName, Project]](Decoder.decodeMap(ProjectName.keyDecodes, Project.decodes(Platform.decodes(platforms))))
          scala <- c.downField("scala").as[Option[Map[ScalaId, Scala]]]
          java <- c.downField("java").as[Option[Java]]
          scripts <- c.downField("scripts").as[Option[Map[ScriptName, JsonList[ScriptDef]]]]
          resolvers <- c.downField("resolvers").as[JsonSet[URI]]
        } yield Build(version, platforms, scala, java, scripts, resolvers, projects)
      )
    implicit val encodes: Encoder[Build] = deriveEncoder
  }

  def parseBuild(json: String): Either[Error, Build] =
    parser.decode[model.Build](json.linesIterator.filterNot(_.trim().startsWith("//")).mkString("\n"))
}
