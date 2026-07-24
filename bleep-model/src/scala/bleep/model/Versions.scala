package bleep.model

/** Every version bleep hardcodes, in one place.
  *
  * These are the versions bleep itself chooses — defaults for new/imported builds, tools it fetches, and artifacts it injects into user classpaths at runtime.
  * They used to be scattered across bleep-model, bleep-core, bleep-cli and bleep-bsp, which let them drift: the BSP server's java-semanticdb fallback and the
  * client's default disagreed, and the zinc compiler-bridge lagged the zinc version in bleep.yaml. Review and bump them here.
  *
  * Not covered: the default JVM (generated into `Jvm.scala` by `bleep.scripts.GenerateResources` — bump it there) and versions users put in their own
  * bleep.yaml.
  */
object Versions {
  // ── Scala ecosystem defaults (new builds, sbt/maven import when unspecified) ──
  val Scala212: String = "2.12.21"
  val Scala213: String = "2.13.18"
  val Scala3: String = "3.8.4"
  val ScalaJs1: String = "1.22.0"
  val ScalaNative05: String = "0.5.12"

  // ── Kotlin ──
  val Kotlin2: String = "2.0.21"
  val Kotlin21: String = "2.1.21"
  val Kotlin23: String = "2.3.21"

  // ── Compilers and formatters bleep fetches ──
  /** Eclipse JDT compiler (ECJ), used when a project opts out of javac */
  val Ecj: String = "3.46.0"
  val GoogleJavaFormat: String = "1.35.0"
  val Ktfmt: String = "0.64"

  /** Written into the example .scalafmt.conf; existing configs pin their own */
  val Scalafmt: String = "3.11.4"

  // ── Runtimes ──
  val Node: String = "24.18.0"

  // ── SemanticDB (IDE support) ──
  /** org.scalameta:semanticdb-scalac — default when the IDE does not request a version. Client default and server fallback both read this. */
  val SemanticdbScalac: String = "4.17.3"

  /** com.sourcegraph:semanticdb-javac — always on the BSP server classpath, see SetupBleepBsp */
  val SemanticdbJavac: String = "0.12.3"

  // ── Zinc ──
  /** org.scala-sbt::compiler-bridge for Scala 2 projects. MUST match the zinc version in bleep.yaml (bleep-bsp's zinc dependency). */
  val CompilerBridge: String = "1.12.0"

  /** The Scala version zinc infrastructure runs on for java-only projects. MUST match bleep-bsp's own scala version (template-scala-3 in bleep.yaml). */
  val ZincScala: String = "3.8.3"

  // ── Test-framework artifacts bleep injects into user test classpaths ──
  /** sbt test interface — 1.0 is final, there will never be another */
  val TestInterface: String = "1.0"
  val JupiterInterface: String = "0.11.1"
  val JunitInterface: String = "0.13.3"

  /** Fallback only: when the project's own classpath already carries a junit-platform, the injected launcher/vintage pair is resolved at THAT version instead
    * (junit hard-fails on any version skew between launcher and engine jars — see externalTestRunnerDeps in MultiWorkspaceBspServer). These defaults apply only
    * to projects with no junit-platform at all, where the pair is internally consistent by construction.
    */
  val JunitPlatformLauncher: String = "1.14.4"
  val JunitVintageEngine: String = "5.14.4"

  // ── Dependencies written into generated builds (bleep build new) ──
  val JunitJupiter: String = "5.14.4"
  val Kotest: String = "6.2.3"
  val Munit: String = "1.3.4"
}
