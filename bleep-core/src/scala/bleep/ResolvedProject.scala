package bleep

import bleep.internal.codecs._
import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

import java.nio.file.Path

/** A resolved project with all dependencies and options expanded.
  *
  * This is bleep's native representation of a fully resolved project, containing everything needed for compilation.
  */
case class ResolvedProject(
    name: String,
    directory: Path,
    workspaceDir: Path,
    sources: List[Path],
    classpath: List[Path],
    classesDir: Path,
    resources: Option[List[Path]],
    language: ResolvedProject.Language,
    platform: Option[ResolvedProject.Platform],
    isTestProject: Boolean,
    dependencies: List[String],
    testFrameworks: List[String],
    resolution: Option[ResolvedProject.Resolution]
)

object ResolvedProject {
  implicit val encodes: Encoder[ResolvedProject] = deriveEncoder

  /** Language configuration - exactly one of Scala, Java, or Kotlin */
  sealed trait Language {
    def javaOptions: List[String]
  }

  object Language {

    /** Pure Java project */
    case class Java(
        options: List[String]
    ) extends Language {
      override def javaOptions: List[String] = options
    }

    /** Scala project (may include Java files for mixed compilation) */
    case class Scala(
        organization: String,
        name: String,
        version: String,
        options: List[String],
        compilerJars: List[Path],
        analysisFile: Option[Path],
        setup: Option[CompileSetup],
        javaOptions: List[String]
    ) extends Language

    /** Kotlin project (may include Java files) */
    case class Kotlin(
        version: String,
        options: List[String],
        compilerJars: List[Path],
        javaOptions: List[String]
    ) extends Language

    implicit val encodesJava: Encoder[Java] = deriveEncoder
    implicit val encodesScala: Encoder[Scala] = deriveEncoder
    implicit val encodesKotlin: Encoder[Kotlin] = deriveEncoder
    implicit val encodes: Encoder[Language] = Encoder.instance {
      case j: Java   => encodesJava(j).deepMerge(io.circe.Json.obj("type" -> io.circe.Json.fromString("java")))
      case s: Scala  => encodesScala(s).deepMerge(io.circe.Json.obj("type" -> io.circe.Json.fromString("scala")))
      case k: Kotlin => encodesKotlin(k).deepMerge(io.circe.Json.obj("type" -> io.circe.Json.fromString("kotlin")))
    }
  }

  case class CompileSetup(
      order: model.CompileOrder,
      addLibraryToBootClasspath: Boolean,
      addCompilerToClasspath: Boolean,
      addExtraJarsToClasspath: Boolean,
      manageBootClasspath: Boolean,
      filterLibraryFromClasspath: Boolean
  )
  object CompileSetup {
    implicit val encodes: Encoder[CompileSetup] = deriveEncoder
  }

  /** Platform configuration */
  sealed trait Platform
  object Platform {
    case class Jvm(
        options: List[String],
        mainClass: Option[String],
        runtimeOptions: List[String]
    ) extends Platform

    case class Js(
        version: String,
        mode: String,
        kind: String,
        emitSourceMaps: Boolean,
        jsdom: Option[Boolean],
        nodePath: Option[Path],
        mainClass: Option[String]
    ) extends Platform

    case class Native(
        version: String,
        mode: String,
        gc: String,
        mainClass: Option[String]
    ) extends Platform

    implicit val encodesJvm: Encoder[Jvm] = deriveEncoder
    implicit val encodesJs: Encoder[Js] = deriveEncoder
    implicit val encodesNative: Encoder[Native] = deriveEncoder
    implicit val encodes: Encoder[Platform] = Encoder.instance {
      case j: Jvm    => encodesJvm(j).deepMerge(io.circe.Json.obj("type" -> io.circe.Json.fromString("jvm")))
      case j: Js     => encodesJs(j).deepMerge(io.circe.Json.obj("type" -> io.circe.Json.fromString("js")))
      case n: Native => encodesNative(n).deepMerge(io.circe.Json.obj("type" -> io.circe.Json.fromString("native")))
    }
  }

  /** Resolution information for dependencies */
  case class Resolution(
      modules: List[ResolvedModule]
  )
  object Resolution {
    implicit val encodes: Encoder[Resolution] = deriveEncoder
  }

  case class ResolvedModule(
      organization: String,
      name: String,
      version: String,
      artifacts: List[ResolvedArtifact]
  )
  object ResolvedModule {
    implicit val encodes: Encoder[ResolvedModule] = deriveEncoder
  }

  case class ResolvedArtifact(
      name: String,
      classifier: Option[String],
      path: Path
  )
  object ResolvedArtifact {
    implicit val encodes: Encoder[ResolvedArtifact] = deriveEncoder
  }

  /** Project tag for IDE integration */
  sealed trait Tag
  object Tag {
    case object Library extends Tag
    case object Test extends Tag
    case object IntegrationTest extends Tag
  }

  def tag(crossName: model.CrossProjectName, isTest: Boolean): Tag =
    if (isTest && crossName.name.value.endsWith("-it")) Tag.IntegrationTest
    else if (isTest) Tag.Test
    else Tag.Library

  /** Helper methods for accessing language-specific configuration */
  implicit class ResolvedProjectOps(val project: ResolvedProject) extends AnyVal {
    def scalaConfig: Option[Language.Scala] = project.language match {
      case s: Language.Scala => Some(s)
      case _                 => None
    }

    def isScalaProject: Boolean = project.language.isInstanceOf[Language.Scala]
    def isJavaProject: Boolean = project.language.isInstanceOf[Language.Java]
    def isKotlinProject: Boolean = project.language.isInstanceOf[Language.Kotlin]
  }
}
