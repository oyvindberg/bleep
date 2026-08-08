package bleep.analysis

import bleep.ResolvedProject

import java.nio.file.Path

/** Language mode for a project */
enum ProjectLanguage {

  /** Scala and Java compiled together by Zinc. `--release` lives in `javaOptions` like any other javac flag — javac honors it from there. `ecjVersion` selects
    * the compiler for the project's `.java` sources, exactly as it does for [[JavaOnly]] — mixed projects honor it too.
    */
  case ScalaJava(
      scalaVersion: String,
      scalaOptions: List[String],
      javaOptions: List[String],
      ecjVersion: Option[String],
      compileOrder: bleep.model.CompileOrder
  )

  /** Kotlin/JVM compiled by K2JVMCompiler */
  case Kotlin(kotlinVersion: String, jvmTarget: String, kotlinOptions: List[String], javaRelease: Option[Int])

  /** Kotlin/JS compiled by K2JSCompiler */
  case KotlinJs(kotlinVersion: String, kotlinOptions: List[String], isTest: Boolean)

  /** Kotlin/Native compiled by K2Native */
  case KotlinNative(kotlinVersion: String, kotlinOptions: List[String], isTest: Boolean)

  /** Java-only compiled by javac or ECJ */
  case JavaOnly(release: Option[Int], javaOptions: List[String], ecjVersion: Option[String])
}

object ProjectLanguage {

  /** Derive a `ScalaJava` from a resolved project. Returns None for non-Scala languages — Kotlin variants etc. don't use the Zinc-backed noop manifest path.
    *
    * `ecjVersion` is not carried by [[ResolvedProject.Language.Scala]], so the caller must supply it from the project's `java` config.
    */
  def fromResolvedScalaJava(resolved: ResolvedProject, ecjVersion: Option[String]): Option[ProjectLanguage.ScalaJava] =
    resolved.language match {
      case scalaLang: ResolvedProject.Language.Scala =>
        Some(
          ProjectLanguage.ScalaJava(
            scalaVersion = scalaLang.version,
            scalaOptions = scalaLang.options,
            javaOptions = scalaLang.javaOptions,
            ecjVersion = ecjVersion,
            compileOrder = scalaLang.setup.map(_.order).getOrElse(bleep.model.CompileOrder.JavaThenScala)
          )
        )
      case _ => None
    }
}

/** Configuration for compiling a project */
case class ProjectConfig(
    name: String,
    sources: Set[Path],
    classpath: Seq[Path],
    outputDir: Path,
    language: ProjectLanguage,
    analysisDir: Option[Path],
    buildDir: Path
)

/** Result of compiling a project */
sealed trait ProjectCompileResult {
  def isSuccess: Boolean
}

case class ProjectCompileSuccess(
    outputDir: Path,
    classFiles: Set[Path],
    analysisFile: Option[Path]
) extends ProjectCompileResult {
  def isSuccess: Boolean = true
}

case class ProjectCompileFailure(
    errors: List[CompilerError]
) extends ProjectCompileResult {
  def isSuccess: Boolean = false
}

/** Compilation was cancelled (build/shutdown, Ctrl-C, kill signal). Distinct from Failure so the BSP compile handler can surface `TaskResult.Killed`, not a
  * synthetic error. Carries the `KillReason` so downstream telemetry can distinguish user-initiated cancels from server shutdowns.
  */
case class ProjectCompileCancelled(reason: bleep.bsp.protocol.KillReason) extends ProjectCompileResult {
  def isSuccess: Boolean = false
}

/** A compiler error with location information */
case class CompilerError(
    path: Option[Path],
    line: Int,
    column: Int,
    message: String,
    rendered: Option[String],
    severity: CompilerError.Severity
) {
  def formatted: String = {
    val loc = path match {
      case Some(p) =>
        val locPart = (line, column) match {
          case (0, 0) => ""
          case (l, 0) => s":$l"
          case (l, c) => s":$l:$c"
        }
        s"${p.getFileName}$locPart"
      case None => "<unknown>"
    }
    s"$loc: $message"
  }
}

object CompilerError {
  enum Severity {
    case Error, Warning, Info
  }
}
