package bleep

import bleep.internal.{EnumCodec, SetLike}
import bloop.config.Config
import coursier.core.Configuration
import coursier.parse.{DependencyParser, JavaOrScalaDependency}
import io.circe._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax._

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
    def isEmpty =
      this match {
        case CompileSetup(None, None, None, None, None, None) => true
        case _                                                => false
      }

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
      options: Option[Options],
      setup: Option[CompileSetup]
  ) extends SetLike[Scala] {
    override def intersect(other: Scala): Scala =
      Scala(
        `extends` = if (`extends` == other.`extends`) `extends` else None,
        version = if (`version` == other.`version`) `version` else None,
        options = options.zip(other.options).map { case (_1, _2) => _1.intersect(_2) }.filterNot(_.isEmpty),
        setup = setup.zip(other.setup).map { case (_1, _2) => _1.intersect(_2) }.filterNot(_.isEmpty)
      )

    override def removeAll(other: Scala): Scala =
      Scala(
        `extends` = if (`extends` == other.`extends`) None else `extends`,
        version = if (`version` == other.`version`) None else `version`,
        options = options.zip(other.options).map { case (_1, _2) => _1.removeAll(_2) }.filterNot(_.isEmpty),
        setup = setup.zip(other.setup).map { case (_1, _2) => _1.removeAll(_2) }.filterNot(_.isEmpty)
      )

    override def union(other: Scala): Scala =
      Scala(
        `extends` = `extends`.orElse(other.`extends`),
        version = version.orElse(other.version),
        options = List(options, other.options).flatten.reduceOption(_ union _),
        setup = List(setup, other.setup).flatten.reduceOption(_ union _)
      )
  }

  object Scala {
    implicit val decodesScala: Decoder[Versions.Scala] = Decoder[String].map(Versions.Scala.apply)
    implicit val encodesScala: Encoder[Versions.Scala] = Encoder[String].contramap(_.scalaVersion)

    val fullDecodes: Decoder[Scala] = deriveDecoder
    val fullEncodes: Encoder[Scala] = deriveEncoder

    val decodeShort = Decoder[ScalaId].map(extends_ => Scala(`extends` = Some(extends_), None, None, None))
    implicit val decodes: Decoder[Scala] =
      decodeShort.or(fullDecodes)
//      Decoder.instance(c =>
//      c.as[Option[ScalaId]].flatMap {
//        case Some(extends_) => Right(Scala(`extends` = Some(extends_), None, None, None))
//        case None           => fullDecodes(c)
//      }
//    )

    implicit val encodes: Encoder[Scala] = Encoder.instance {
      case Scala(Some(extends_), None, None, None) => Encoder[ScalaId].apply(extends_)
      case full                                    => fullEncodes(full)
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

  sealed abstract class Platform(val name: PlatformId) {
    def `extends`: Option[PlatformId]
  }

  object Platform {
    type ReduceOf[P] = (P, P) => P

    def unsafeReduce(one: Platform, two: Platform)(js: ReduceOf[Js], jvm: ReduceOf[Jvm], native: ReduceOf[Native]): Platform =
      (one, two) match {
        case (one: Js, two: Js)         => js(one, two)
        case (one: Jvm, two: Jvm)       => jvm(one, two)
        case (one: Native, two: Native) => native(one, two)
        case (one, two)                 => sys.error(s"Cannot mix ${one.name.value} and ${two.name.value}")
      }

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
    ) extends Platform(PlatformId("js"))
        with SetLike[Js] {
      override def intersect(other: Js): Js =
        Js(
          `extends` = if (`extends` == other.`extends`) `extends` else None,
          version = if (version == other.version) version else None,
          mode = if (mode == other.mode) mode else None,
          kind = if (kind == other.kind) kind else None,
          emitSourceMaps = if (emitSourceMaps == other.emitSourceMaps) emitSourceMaps else None,
          jsdom = if (jsdom == other.jsdom) jsdom else None,
          mainClass = if (mainClass == other.mainClass) mainClass else None
        )

      override def removeAll(other: Js): Js =
        Js(
          `extends` = if (`extends` == other.`extends`) None else `extends`,
          version = if (version == other.version) None else version,
          mode = if (mode == other.mode) None else mode,
          kind = if (kind == other.kind) None else kind,
          emitSourceMaps = if (emitSourceMaps == other.emitSourceMaps) None else emitSourceMaps,
          jsdom = if (jsdom == other.jsdom) None else jsdom,
          mainClass = if (mainClass == other.mainClass) None else mainClass
        )

      override def union(other: Js): Js =
        Js(
          `extends` = `extends`.orElse(other.`extends`),
          version = version.orElse(other.version),
          mode = mode.orElse(other.mode),
          kind = kind.orElse(other.kind),
          emitSourceMaps = emitSourceMaps.orElse(other.emitSourceMaps),
          jsdom = jsdom.orElse(other.jsdom),
          mainClass = mainClass.orElse(other.mainClass)
        )
    }

    case class Jvm(
        `extends`: Option[PlatformId],
        //      home: Option[Path],
        options: Option[Options],
        mainClass: Option[String],
        //      runtimeHome: Option[Path],
        runtimeOptions: Option[Options]
        //        classpath: Option[List[Path]],
        //        resources: Option[List[Path]]
    ) extends Platform(PlatformId("jvm"))
        with SetLike[Jvm] {
      override def intersect(other: Jvm): Jvm =
        Jvm(
          `extends` = if (`extends` == other.`extends`) `extends` else None,
          options = List(options, other.options).flatten.reduceOption(_.intersect(_)),
          mainClass = if (mainClass == other.mainClass) mainClass else None,
          runtimeOptions = List(runtimeOptions, other.runtimeOptions).flatten.reduceOption(_.intersect(_))
        )

      override def removeAll(other: Jvm): Jvm =
        Jvm(
          `extends` = if (`extends` == other.`extends`) None else `extends`,
          options = List(options, other.options).flatten.reduceOption(_.removeAll(_)),
          mainClass = if (mainClass == other.mainClass) None else mainClass,
          runtimeOptions = List(runtimeOptions, other.runtimeOptions).flatten.reduceOption(_.removeAll(_))
        )

      override def union(other: Jvm): Jvm =
        Jvm(
          `extends` = `extends`.orElse(other.`extends`),
          options = List(options, other.options).flatten.reduceOption(_ union _),
          mainClass = mainClass.orElse(other.mainClass),
          runtimeOptions = List(runtimeOptions, other.runtimeOptions).flatten.reduceOption(_ union _)
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
    ) extends Platform(PlatformId("native"))
        with SetLike[Native] {
      override def intersect(other: Native): Native =
        Native(
          `extends` = if (`extends` == other.`extends`) `extends` else None,
          version = if (version == other.version) version else None,
          mode = if (mode == other.mode) mode else None,
          gc = if (gc == other.gc) gc else None,
          mainClass = if (mainClass == other.mainClass) mainClass else None
        )

      override def removeAll(other: Native): Native =
        Native(
          `extends` = if (`extends` == other.`extends`) None else `extends`,
          version = if (version == other.version) None else version,
          mode = if (mode == other.mode) None else mode,
          gc = if (gc == other.gc) None else gc,
          mainClass = if (mainClass == other.mainClass) None else mainClass
        )

      override def union(other: Native): Native =
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

    def decodes(platforms: Option[Map[PlatformId, Platform]]): Decoder[Platform] = {
      platforms match {
        case Some(platforms) =>
          val decodeShort = PlatformId.decodes.emap { id =>
            platforms.get(id) match {
              case Some(Js(_, _, _, _, _, _, _)) => Right(Js(Some(id), None, None, None, None, None, None))
              case Some(Jvm(_, _, _, _))         => Right(Jvm(Some(id), None, None, None))
              case Some(Native(_, _, _, _, _))   => Right(Native(Some(id), None, None, None, None))
              case None                          => Left(s"${id.value} is not a defined platform")
            }
          }

          decodeShort.or(decodeFull)

        case None => decodeFull
      }
    }

    implicit val encodes: Encoder[Platform] = Encoder.instance {
      case Js(Some(extends_), None, None, None, None, None, None) => extends_.asJson
      case platform @ (x: Js)                                     => encodesJs(x).mapObject(_.add("name", platform.name.asJson))
      case Jvm(Some(extends_), None, None, None)                  => extends_.asJson
      case platform @ (x: Jvm)                                    => encodesJvm(x).mapObject(_.add("name", platform.name.asJson))
      case Native(Some(extends_), None, None, None, None)         => extends_.asJson
      case platform @ (x: Native)                                 => encodesNative(x).mapObject(_.add("name", platform.name.asJson))
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
      projects: Map[ProjectName, Project],
      platforms: Option[Map[PlatformId, Platform]],
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

    implicit def decodes: Decoder[Build] = {
      Decoder.instance(c =>
        for {
          version <- c.downField("version").as[String]
          platforms <- c.downField("platforms").as(Decoder.decodeOption(Decoder.decodeMap(PlatformId.keyDecodes, Platform.decodes(None))))
          projects <- c.downField("projects").as[Map[ProjectName, Project]](Decoder.decodeMap(ProjectName.keyDecodes, Project.decodes(Platform.decodes(platforms))))
          scala <- c.downField("scala").as[Option[Map[ScalaId, Scala]]]
          java <- c.downField("java").as[Option[Java]]
          scripts <- c.downField("scripts").as[Option[Map[ScriptName, JsonList[ScriptDef]]]]
          resolvers <- c.downField("resolvers").as[Option[JsonList[URI]]]
        } yield Build(version, projects, platforms, scala, java, scripts, resolvers)
      )
    }
    implicit val encodes: Encoder[Build] = deriveEncoder
  }

  def parseBuild(json: String): Either[Error, Build] =
    parser.decode[model.Build](json)
}
