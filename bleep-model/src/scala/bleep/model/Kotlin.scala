package bleep.model

import bleep.internal.compat.*
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Kotlin configuration for a project.
  *
  * @param version
  *   The Kotlin version (e.g., "2.0.0", "2.1.0")
  * @param options
  *   Kotlin compiler options (e.g., "-Xjsr305=strict")
  * @param jvmTarget
  *   JVM bytecode target version (e.g., "11", "17", "21")
  * @param compilerPlugins
  *   Kotlin compiler plugin IDs (e.g., "spring", "jpa", "allopen", "noarg", "serialization"). These are resolved to JARs at compile time using the project's
  *   Kotlin version.
  * @param kspVersion
  *   The KSP-side version suffix (e.g. "1.0.29"). Bleep concatenates this with [[version]] to form the full coord
  *   `com.google.devtools.ksp:symbol-processing-aa-embeddable:<kotlin>-<ksp>`. Required when [[symbolProcessors]] is non-empty or [[scanForSymbolProcessors]]
  *   is true. No default — fail loud at resolution if missing. KSP releases are pinned 1:1 to exact kotlinc versions, so [[version]] determines the prefix;
  *   this field only carries the KSP-side suffix.
  * @param scanForSymbolProcessors
  *   When set to true, bleep scans every resolved-`dependencies` jar for `META-INF/services/com.google.devtools.ksp.processing.SymbolProcessorProvider` and
  *   adds matching jars to KSP's processor classpath. Default off — explicit opt-in.
  * @param symbolProcessors
  *   KSP processor-only deps. Resolved separately and passed as the trailing positional classpath to the standalone `KSPJvmMain` runner — never on the
  *   project's runtime classpath. Composes with [[scanForSymbolProcessors]] when both are set.
  * @param symbolProcessorOptions
  *   `<key>=<value>` flags passed to KSP via `-processor-options=`. Every configured processor can see every option (no per-processor scoping).
  * @param js
  *   Kotlin/JS specific configuration
  * @param native
  *   Kotlin/Native specific configuration
  */
case class Kotlin(
    version: Option[VersionKotlin],
    options: Options,
    jvmTarget: Option[String],
    compilerPlugins: JsonSet[String],
    kspVersion: Option[String],
    scanForSymbolProcessors: Option[Boolean],
    symbolProcessors: JsonSet[Dep],
    symbolProcessorOptions: SymbolProcessorOptions,
    js: Option[KotlinJs],
    native: Option[KotlinNative]
) extends SetLike[Kotlin] {

  override def intersect(other: Kotlin): Kotlin =
    Kotlin(
      version = if (version == other.version) version else None,
      options = options.intersect(other.options),
      jvmTarget = if (jvmTarget == other.jvmTarget) jvmTarget else None,
      compilerPlugins = compilerPlugins.intersect(other.compilerPlugins),
      kspVersion = if (kspVersion == other.kspVersion) kspVersion else None,
      scanForSymbolProcessors = if (scanForSymbolProcessors == other.scanForSymbolProcessors) scanForSymbolProcessors else None,
      symbolProcessors = symbolProcessors.intersect(other.symbolProcessors),
      symbolProcessorOptions = symbolProcessorOptions.intersect(other.symbolProcessorOptions),
      js = js.zipCompat(other.js).map { case (a, b) => a.intersect(b) },
      native = native.zipCompat(other.native).map { case (a, b) => a.intersect(b) }
    )

  override def removeAll(other: Kotlin): Kotlin =
    Kotlin(
      version = if (version == other.version) None else version,
      options = options.removeAll(other.options),
      jvmTarget = if (jvmTarget == other.jvmTarget) None else jvmTarget,
      compilerPlugins = compilerPlugins.removeAll(other.compilerPlugins),
      kspVersion = if (kspVersion == other.kspVersion) None else kspVersion,
      scanForSymbolProcessors = if (scanForSymbolProcessors == other.scanForSymbolProcessors) None else scanForSymbolProcessors,
      symbolProcessors = symbolProcessors.removeAll(other.symbolProcessors),
      symbolProcessorOptions = symbolProcessorOptions.removeAll(other.symbolProcessorOptions),
      js = removeAllFrom(js, other.js),
      native = removeAllFrom(native, other.native)
    )

  override def union(other: Kotlin): Kotlin =
    Kotlin(
      version = version.orElse(other.version),
      options = options.union(other.options),
      jvmTarget = jvmTarget.orElse(other.jvmTarget),
      compilerPlugins = compilerPlugins.`union`(other.compilerPlugins),
      kspVersion = kspVersion.orElse(other.kspVersion),
      scanForSymbolProcessors = scanForSymbolProcessors.orElse(other.scanForSymbolProcessors),
      symbolProcessors = symbolProcessors.`union`(other.symbolProcessors),
      symbolProcessorOptions = symbolProcessorOptions.`union`(other.symbolProcessorOptions),
      js = List(js, other.js).flatten.reduceOption(_ `union` _),
      native = List(native, other.native).flatten.reduceOption(_ `union` _)
    )

  override def isEmpty: Boolean =
    version.isEmpty && options.isEmpty && jvmTarget.isEmpty && compilerPlugins.isEmpty &&
      kspVersion.isEmpty && scanForSymbolProcessors.isEmpty && symbolProcessors.isEmpty && symbolProcessorOptions.isEmpty &&
      js.forall(_.isEmpty) && native.forall(_.isEmpty)

  /** True iff this project is configured to run KSP symbol processors. Used to decide whether to schedule a [[ResolveSymbolProcessorsTask]] for the project. */
  def hasSymbolProcessing: Boolean =
    scanForSymbolProcessors.contains(true) || symbolProcessors.values.nonEmpty

  /** Helper to remove all SetLike elements */
  private def removeAllFrom[T <: SetLike[T]](a: Option[T], b: Option[T]): Option[T] =
    (a, b) match {
      case (Some(aVal), Some(bVal)) => Some(aVal.removeAll(bVal)).filterNot(_.isEmpty)
      case _                        => a
    }
}

object Kotlin {
  val empty: Kotlin = Kotlin(
    version = None,
    options = Options.empty,
    jvmTarget = None,
    compilerPlugins = JsonSet.empty,
    kspVersion = None,
    scanForSymbolProcessors = None,
    symbolProcessors = JsonSet.empty,
    symbolProcessorOptions = SymbolProcessorOptions.empty,
    js = None,
    native = None
  )

  implicit val decodes: Decoder[Kotlin] = deriveDecoder
  implicit val encodes: Encoder[Kotlin] = deriveEncoder
}
