package bleep

import bleep.model.IgnoreEvictionErrors
import ryddig.Logger

import java.io.File
import java.nio.file.Path
import scala.collection.immutable.{SortedMap, SortedSet}

/** The configuration for one project's KSP run via the standalone `symbol-processing-aa-embeddable` runner. Carries the runner classpath (everything the runner
  * JVM needs to load — KSP + Analysis API + Kotlin stdlib + transitive), the user-listed processor classpath (passed as the trailing positional argument), the
  * output directory layout, and the processor `apoption=k=v` flags. The resolver produces this struct; [[KspRunner]] consumes it to build the
  * `java -cp ... com.google.devtools.ksp.cmdline.KSPJvmMain ...` invocation.
  *
  * @param runnerClasspath
  *   Every JAR the KSP runner JVM needs on its classpath. The full transitive closure of `symbol-processing-aa-embeddable`.
  * @param processorJars
  *   User-listed (or scanned) KSP processor JARs. Become the trailing positional `<processor classpath>` arg.
  * @param librariesClasspath
  *   The project's compile classpath (third-party JARs + upstream project class dirs). KSP uses this to resolve types referenced in processed code. Maps to the
  *   `-libraries=` argument.
  * @param sourceRoots
  *   Kotlin source roots. Maps to `-source-roots=`.
  * @param javaSourceRoots
  *   Java source roots (KSP can also read Java symbols). Maps to `-java-source-roots=`.
  * @param moduleName
  *   The Kotlin module name for this project. Maps to `-module-name=`.
  * @param jvmTarget
  *   `-jvm-target=` value, e.g. "11", "17", "21".
  * @param languageVersion
  *   `-language-version=` value (e.g. "1.9", "2.0"). KSP requires this even though it has its own bundled kotlinc; this controls Analysis API symbol semantics.
  * @param apiVersion
  *   `-api-version=` value. Usually equal to languageVersion.
  * @param jdkHome
  *   Path to the JDK used for Java symbol resolution. Maps to `-jdk-home=`.
  * @param projectBaseDir
  *   The project's source directory. Used by KSP for relative-path resolution. Maps to `-project-base-dir=`.
  * @param kspOutputBaseDir
  *   `.bleep/projects/<cross>/generated-sources/ksp/`. Maps to `-output-base-dir=`. Source-like outputs (`kotlin/`, `java/`, `resources/`) live underneath.
  * @param kotlinOutputDir
  *   `<kspOutputBaseDir>/kotlin/`. Maps to `-kotlin-output-dir=`.
  * @param javaOutputDir
  *   `<kspOutputBaseDir>/java/`. Maps to `-java-output-dir=`.
  * @param classOutputDir
  *   `.bleep/projects/<cross>/builds/<variant>/ksp/classes/`. Per-variant so a BSP and a Normal compile don't overwrite each other's bytecode.
  * @param resourceOutputDir
  *   `<kspOutputBaseDir>/resources/`. Maps to `-resource-output-dir=`.
  * @param cachesDir
  *   `.bleep/projects/<cross>/builds/<variant>/ksp/caches/`. Per-variant; KSP's own incremental cache.
  * @param processorOptions
  *   Per-processor `<k>=<v>` flags. Passed via KSP's `-processor-options=` argument and joined with the platform path separator (`:` on Unix, `;` on Windows).
  *   A value containing the separator will be misparsed by KSP on the receiving side — a known limitation that hasn't bitten users yet.
  */
case class SymbolProcessorResult(
    runnerClasspath: List[Path],
    processorJars: List[Path],
    librariesClasspath: List[Path],
    sourceRoots: List[Path],
    javaSourceRoots: List[Path],
    moduleName: String,
    jvmTarget: String,
    languageVersion: String,
    apiVersion: String,
    jdkHome: Path,
    projectBaseDir: Path,
    kspOutputBaseDir: Path,
    kotlinOutputDir: Path,
    javaOutputDir: Path,
    classOutputDir: Path,
    resourceOutputDir: Path,
    cachesDir: Path,
    processorOptions: SortedMap[String, String]
) {

  /** Build the `KSPJvmMain` argv. The decision parameter governs whether KSP runs incremental (and with what modified/removed-sources lists) or full. Order:
    * every `-name=value` arg first, then the processor classpath as the trailing positional. Path lists use the platform's path separator (`:` on Unix, `;` on
    * Windows) which matches KSP's documented format.
    */
  def runnerArgs(decision: KspIncrementalState.Decision): List[String] = {
    val sep = File.pathSeparator
    val processorOptStr =
      if (processorOptions.isEmpty) None
      else Some(processorOptions.iterator.map { case (k, v) => s"$k=$v" }.mkString(sep))

    val args = List.newBuilder[String]
    args += s"-module-name=$moduleName"
    args += s"-source-roots=${sourceRoots.iterator.map(_.toString).mkString(sep)}"
    if (javaSourceRoots.nonEmpty)
      args += s"-java-source-roots=${javaSourceRoots.iterator.map(_.toString).mkString(sep)}"
    if (librariesClasspath.nonEmpty)
      args += s"-libraries=${librariesClasspath.iterator.map(_.toString).mkString(sep)}"
    args += s"-jdk-home=$jdkHome"
    args += s"-jvm-target=$jvmTarget"
    args += s"-project-base-dir=$projectBaseDir"
    args += s"-output-base-dir=$kspOutputBaseDir"
    args += s"-caches-dir=$cachesDir"
    args += s"-class-output-dir=$classOutputDir"
    args += s"-kotlin-output-dir=$kotlinOutputDir"
    args += s"-java-output-dir=$javaOutputDir"
    args += s"-resource-output-dir=$resourceOutputDir"
    args += s"-language-version=$languageVersion"
    args += s"-api-version=$apiVersion"
    decision match {
      case KspIncrementalState.Decision.FullRebuild | KspIncrementalState.Decision.CacheBust =>
        args += "-incremental=false"
      case KspIncrementalState.Decision.Incremental(modified, removed) =>
        args += "-incremental=true"
        if (modified.nonEmpty) args += s"-modified-sources=${modified.iterator.map(_.toString).mkString(sep)}"
        if (removed.nonEmpty) args += s"-removed-sources=${removed.iterator.map(_.toString).mkString(sep)}"
    }
    processorOptStr.foreach(s => args += s"-processor-options=$s")
    args += processorJars.iterator.map(_.toString).mkString(sep)
    args.result()
  }
}

object SymbolProcessorResolver {

  /** Resolve KSP processors and the standalone runner classpath for a single project. Four input signals on `model.Kotlin`:
    *   - `version`: provides the kotlinc-version prefix for the KSP coord
    *   - `kspVersion`: required; the KSP-side version suffix (e.g. "1.0.32")
    *   - `scanForSymbolProcessors`: opt-in to scan resolved-`dependencies` jars for KSP processor jars
    *   - `symbolProcessors`: explicit processor-only deps
    *   - `symbolProcessorOptions`: per-processor `apoption=k=v` flags
    *
    * Loud-fails when:
    *   - `kotlin.version` is unset (the KSP coord needs the kotlin prefix)
    *   - `kotlin.kspVersion` is unset
    *   - `scanForSymbolProcessors: true` is set but neither scanning nor the explicit list yields any processor jars (no-op opt-in)
    *   - The KSP runner coord doesn't resolve (handled by [[bleep.analysis.CompilerResolver.resolveKspPlugin]])
    *
    * @param resolveKspPlugin
    *   Function from (kotlinVersion, kspVersion) to the resolved KSP runner classpath. Indirected so this resolver lives in bleep-core without depending on
    *   bleep-bsp's analysis module.
    */
  def resolve(
      crossName: model.CrossProjectName,
      kotlin: model.Kotlin,
      resolvedDependencyJars: List[Path],
      librariesClasspath: List[Path],
      sourceRoots: List[Path],
      javaSourceRoots: List[Path],
      moduleName: String,
      jvmTarget: String,
      jdkHome: Path,
      versionCombo: model.VersionCombo,
      libraryVersionSchemes: SortedSet[model.LibraryVersionScheme],
      resolver: CoursierResolver,
      projectBaseDir: Path,
      kspSharedOutputBaseDir: Path,
      kspVariantStateDir: Path,
      resolveKspPlugin: (model.VersionKotlin, String) => Seq[Path],
      logger: Logger
  ): SymbolProcessorResult = {
    val kotlinVersion = kotlin.version.getOrElse {
      sys.error(s"project ${crossName.value}: kotlin.version is required when symbol processors are configured")
    }
    val kspVersion = kotlin.kspVersion.getOrElse {
      sys.error(
        s"project ${crossName.value}: kotlin.kspVersion is required when symbolProcessors / scanForSymbolProcessors is set. " +
          s"See https://github.com/google/ksp/releases for the version paired with kotlin ${kotlinVersion.kotlinVersion}."
      )
    }
    if (!kspVersion.matches("""\d+\.\d+\.\d+"""))
      sys.error(s"project ${crossName.value}: kotlin.kspVersion='$kspVersion' must be of form 'x.y.z' (KSP suffix only; bleep prepends the kotlin version).")

    val wantsScan = kotlin.scanForSymbolProcessors.contains(true)
    val explicitDeps: Set[model.Dep] = kotlin.symbolProcessors.values.toSet

    val explicitJars: List[Path] = explicitDeps.toList.flatMap { dep =>
      val mapped = dep.mapScala(_.copy(forceJvm = true))
      resolver
        .force(
          Set(mapped),
          versionCombo,
          libraryVersionSchemes,
          s"${crossName.value}/symbolProcessor",
          IgnoreEvictionErrors.No
        )
        .jars
    }

    val scannedJars: List[Path] =
      if (!wantsScan) Nil
      else
        resolvedDependencyJars.flatMap { jarPath =>
          symbolProcessorProviderClasses(jarPath).filter(_.nonEmpty) match {
            case Some(classes) =>
              logger.info(s"[${crossName.value}] auto-discovered KSP processor JAR: $jarPath — providers: ${classes.mkString(", ")}")
              Some(jarPath)
            case None => None
          }
        }

    if (wantsScan && explicitJars.isEmpty && scannedJars.isEmpty)
      sys.error(
        s"project ${crossName.value}: scanForSymbolProcessors: true was set but no KSP processor JARs were found in dependencies and symbolProcessors is empty"
      )

    // When scanning, include the full resolved-dependencies classpath on the processor classpath, not just the scan-matched jar. KSP's standalone runner loads
    // processors in an isolated URLClassLoader — a processor's static initializer that references a class from a transitive dep (e.g. moshi-kotlin-codegen
    // referencing `com.squareup.moshi.JsonClass`) needs that dep on the same classloader. The transitive dep is on the user's `dependencies:`, so passing the
    // entire resolved-dependency classpath is the simplest correct answer. For explicit `symbolProcessors:`, each entry is resolved with its own transitive
    // closure already (see `explicitJars` above) so we don't need the full dep classpath there.
    val processorJars: List[Path] =
      if (wantsScan && scannedJars.nonEmpty) (resolvedDependencyJars ++ explicitJars).distinct
      else (scannedJars ++ explicitJars).distinct
    val runnerClasspath: List[Path] = resolveKspPlugin(kotlinVersion, kspVersion).toList

    // KSP's Analysis-API runner accepts the same `-language-version` / `-api-version` values as kotlinc. Default to "1.9" which is widely compatible; users
    // can override by setting `-language-version=...` in `kotlin.options` (parsed below).
    val (languageVersion, apiVersion) = extractLanguageApi(kotlin.options.render).getOrElse(("1.9", "1.9"))

    SymbolProcessorResult(
      runnerClasspath = runnerClasspath,
      processorJars = processorJars,
      librariesClasspath = librariesClasspath,
      sourceRoots = sourceRoots,
      javaSourceRoots = javaSourceRoots,
      moduleName = moduleName,
      jvmTarget = jvmTarget,
      languageVersion = languageVersion,
      apiVersion = apiVersion,
      jdkHome = jdkHome,
      projectBaseDir = projectBaseDir,
      // KSP wants one base for its `-output-base-dir`; per-source-kind dirs ride either side. Sources go shared (across variants), bytecode + cache go
      // per-variant. The `kspOutputBaseDir` slot is used by KSP mostly as a label; the individual `-kotlin-output-dir` / `-class-output-dir` overrides win.
      kspOutputBaseDir = kspSharedOutputBaseDir,
      kotlinOutputDir = kspSharedOutputBaseDir.resolve("kotlin"),
      javaOutputDir = kspSharedOutputBaseDir.resolve("java"),
      resourceOutputDir = kspSharedOutputBaseDir.resolve("resources"),
      classOutputDir = kspVariantStateDir.resolve("classes"),
      cachesDir = kspVariantStateDir.resolve("caches"),
      processorOptions = SortedMap.from(kotlin.symbolProcessorOptions.value)
    )
  }

  /** Extract `-language-version` and `-api-version` from a project's kotlin.options. Both kotlinc styles are supported: `-language-version=2.0` and the
    * separate-token form `-language-version 2.0`. Returns None if neither is set.
    */
  private def extractLanguageApi(rendered: List[String]): Option[(String, String)] = {
    val langOpt = findValue(rendered, "-language-version")
    val apiOpt = findValue(rendered, "-api-version")
    (langOpt, apiOpt) match {
      case (Some(lang), Some(api)) => Some((lang, api))
      case (Some(lang), None)      => Some((lang, lang)) // match kotlinc default: api-version inherits from language-version
      case (None, Some(api))       => Some((api, api))
      case (None, None)            => None
    }
  }

  private def findValue(opts: List[String], flag: String): Option[String] = {
    val eq = s"$flag="
    opts.iterator
      .map(_.trim)
      .collectFirst {
        case o if o.startsWith(eq) => o.stripPrefix(eq)
      }
      .orElse {
        opts.sliding(2).collectFirst { case List(a, b) if a == flag => b }
      }
  }

  /** When `jarPath` declares KSP processors via ServiceLoader registration (i.e. has a
    * `META-INF/services/com.google.devtools.ksp.processing.SymbolProcessorProvider` entry), returns the parsed list of fully-qualified class names. `None`
    * means the jar has no service file. `Some(Nil)` means the file exists but is empty / comment-only.
    */
  def symbolProcessorProviderClasses(jarPath: Path): Option[List[String]] =
    if (!java.nio.file.Files.isRegularFile(jarPath)) None
    else {
      val zip = new java.util.zip.ZipFile(jarPath.toFile)
      try {
        val entry = zip.getEntry("META-INF/services/com.google.devtools.ksp.processing.SymbolProcessorProvider")
        if (entry == null) None
        else {
          val src = scala.io.Source.fromInputStream(zip.getInputStream(entry), "UTF-8")
          try {
            val classes = src
              .getLines()
              .map(line => line.indexOf('#') match { case -1 => line; case i => line.substring(0, i) })
              .map(_.trim)
              .filter(_.nonEmpty)
              .toList
            Some(classes)
          } finally src.close()
        }
      } finally zip.close()
    }
}
