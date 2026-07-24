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
  val Scala213: String = "2.13.16"
  val Scala3: String = "3.8.3"
  val ScalaJs1: String = "1.19.0"
  val ScalaNative05: String = "0.5.6"

  // ── Kotlin ──
  val Kotlin2: String = "2.0.0"
  val Kotlin21: String = "2.1.0"
  val Kotlin23: String = "2.3.0"

  // ── Compilers and formatters bleep fetches ──
  /** Eclipse JDT compiler (ECJ), used when a project opts out of javac */
  val Ecj: String = "3.44.0"
  val GoogleJavaFormat: String = "1.35.0"
  val Ktfmt: String = "0.62"

  /** Written into the example .scalafmt.conf; existing configs pin their own */
  val Scalafmt: String = "3.11.1"

  // ── Runtimes ──
  val Node: String = "24.16.0"

  // ── SemanticDB (IDE support) ──
  /** org.scalameta:semanticdb-scalac — default when the IDE does not request a version. Client default and server fallback both read this. */
  val SemanticdbScalac: String = "4.15.2"

  /** com.sourcegraph:semanticdb-javac — always on the BSP server classpath, see SetupBleepBsp */
  val SemanticdbJavac: String = "0.11.2"

  // ── Zinc ──
  /** org.scala-sbt::compiler-bridge for Scala 2 projects. MUST match the zinc version in bleep.yaml (bleep-bsp's zinc dependency). */
  val CompilerBridge: String = "1.10.4"

  /** The Scala version zinc infrastructure runs on for java-only projects. MUST match bleep-bsp's own scala version (template-scala-3 in bleep.yaml). */
  val ZincScala: String = "3.8.3"

  // ── Test-framework artifacts bleep injects into user test classpaths ──
  /** sbt test interface — 1.0 is final, there will never be another */
  val TestInterface: String = "1.0"
  val JupiterInterface: String = "0.11.1"
  val JunitInterface: String = "0.13.3"
  val JunitPlatformLauncher: String = "1.9.1"
  val JunitVintageEngine: String = "5.9.1"

  // ── Dependencies written into generated builds (bleep build new) ──
  val JunitJupiter: String = "5.10.1"
  val Kotest: String = "5.8.0"
  val Munit: String = "1.0.0"
}
