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
      compileOrder: bleep.model.CompileOrder,
      compilerProject: Option[ResolvedProject.Language.ProjectCompiler]
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
            compileOrder = scalaLang.setup.map(_.order).getOrElse(bleep.model.CompileOrder.JavaThenScala),
            compilerProject = scalaLang.compilerProject
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
    buildDir: Path,
    determinants: OutputDeterminants
)

/** What decides a project's output besides its sources, options and classpath — the things incremental compilation cannot see on its own.
  *
  *   - [[compiler]]: the compiler, when it is built in this build (`scala.compilerProject`). A new compiler may compile any source differently, so this is a
  *     full recompile. Identified by the compiler project's content digest, so it is the same on every machine and a remote-cached project stays valid wherever
  *     it is pulled. Passed to zinc as an `extra` setup pair.
  *   - [[transformedClasses]]: the classes of dependencies that a post-compile transform changed, added or removed. Consumers compile against the transform's
  *     output, but zinc's analysis of the dependency describes the compiler's. These are handed to zinc at its external lookup, folded into each class's
  *     hashes, so zinc invalidates exactly the consumer sources that use a class the transform changed — the same way it does for a source change.
  *
  * Both are in the noop manifest's hash; a difference makes zinc look.
  */
case class OutputDeterminants(compiler: Option[String], transformedClasses: Map[String, TransformedClass]) {
  def asZincExtra: List[(String, String)] = compiler.map("bleep.compiler" -> _).toList
}

object OutputDeterminants {
  val none: OutputDeterminants = OutputDeterminants(compiler = None, transformedClasses = Map.empty)
}

/** A class a post-compile transform changed, added or removed — its effect on the API consumers compile against, as [[kind]] and [[hash]].
  *
  * @param binaryName
  *   the JVM binary name, `lib.Outer$Inner`
  * @param hash
  *   of the API facts the transform added and removed for this class; changes exactly when the transform's effect on its API does
  * @param names
  *   every member name the transformed class exposes, and its simple name: the names a consumer can have used, so a change reaches every one of them
  */
case class TransformedClass(binaryName: String, kind: TransformedClass.Kind, hash: String, names: List[String])

object TransformedClass {
  sealed abstract class Kind(val value: String)
  object Kind {
    case object Changed extends Kind("changed")
    case object Added extends Kind("added")
    case object Removed extends Kind("removed")
    val All: List[Kind] = List(Changed, Added, Removed)
  }

  /** One class per line: kind, binary name, hash, then its names — tab-separated. No compiler emits a tab in a class or member name, and bleep writes and reads
    * both ends.
    */
  def write(classes: List[TransformedClass]): String =
    classes.sortBy(_.binaryName).map(c => (c.kind.value :: c.binaryName :: c.hash :: c.names).mkString("\t")).mkString("", "\n", "\n")

  def read(content: String): List[TransformedClass] =
    content.linesIterator.filter(_.nonEmpty).toList.map { line =>
      line.split('\t').toList match {
        case kind :: binaryName :: hash :: names =>
          TransformedClass(
            binaryName,
            Kind.All.find(_.value == kind).getOrElse(throw new IllegalArgumentException(s"unknown kind '$kind' in: $line")),
            hash,
            names
          )
        case _ => throw new IllegalArgumentException(s"malformed transformed-class line: $line")
      }
    }
}

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
