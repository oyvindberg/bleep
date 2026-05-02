package bleep

import bleep.model.IgnoreEvictionErrors
import ryddig.Logger

import java.io.File
import java.nio.file.Path
import scala.collection.immutable.{SortedMap, SortedSet}

/** The javac flags derived from a project's annotation-processor configuration: jars on `-processorpath`, the generated-sources directory for `-s`, and any
  * `-A<k>=<v>` flags. Bundled so the AP DAG handler can write a single value into the shared per-build map and the compile handler can read it without
  * re-deriving anything.
  */
case class AnnotationProcessorResult(
    processorJars: List[Path],
    genSourcesDir: Path,
    aFlags: SortedMap[String, String]
) {
  def javacFlags: List[String] = {
    val processorPath = processorJars.mkString(File.pathSeparator)
    val aArgs = aFlags.iterator.map { case (k, v) => s"-A$k=$v" }.toList
    List("-processorpath", processorPath, "-s", genSourcesDir.toString) ++ aArgs
  }
}

object AnnotationProcessorResolver {

  /** Resolve annotation processors for a single project. Three input signals on `model.Java`:
    *   - `scanForAnnotationProcessors`: opt-in to scan resolved-`dependencies` jars for processor jars
    *   - `annotationProcessors`: explicit processor-only deps
    *   - `annotationProcessorOptions`: `-A<k>=<v>` flags
    *
    * Loud-fails (`sys.error`) when:
    *   - `scanForAnnotationProcessors: true` is set but neither scanning nor the explicit list yields any processor jars (no-op opt-in)
    *   - the user wrote conflicting flags into `java.options` (manual `-processorpath`, `-A`, `-proc:`, or ` -s `)
    *
    * The escape hatch (`-proc:none` already in `java.options`) is NOT handled here — callers should detect that case and skip calling `resolve` entirely.
    */
  def resolve(
      crossName: model.CrossProjectName,
      java: model.Java,
      resolvedDependencyJars: List[Path],
      versionCombo: model.VersionCombo,
      libraryVersionSchemes: SortedSet[model.LibraryVersionScheme],
      resolver: CoursierResolver,
      genSourcesDir: Path,
      logger: Logger
  ): AnnotationProcessorResult = {
    val rendered = java.options.values.mkString(" ")
    if (rendered.contains("-proc:") || rendered.contains(" -s "))
      sys.error(
        s"project ${crossName.value}: cannot use manual -proc:* or -s in java.options when annotation processing is configured (set scanForAnnotationProcessors / annotationProcessors instead)"
      )
    if (rendered.contains("-processorpath"))
      sys.error(s"project ${crossName.value}: cannot use manual -processorpath in java.options when annotation processing is configured")
    if (java.options.values.exists(_.render.exists(_.startsWith("-A"))))
      sys.error(
        s"project ${crossName.value}: cannot use manual -A flags in java.options when annotation processing is configured (use annotationProcessorOptions)"
      )

    val wantsScan = java.scanForAnnotationProcessors.contains(true)
    val explicitDeps: Set[model.Dep] = java.annotationProcessors.values.toSet

    val explicitJars: List[Path] = explicitDeps.toList.flatMap { dep =>
      val mapped = dep.mapScala(_.copy(forceJvm = true))
      resolver
        .force(
          Set(mapped),
          versionCombo,
          libraryVersionSchemes,
          s"${crossName.value}/annotationProcessor",
          IgnoreEvictionErrors.No
        )
        .jars
    }

    val scannedJars: List[Path] =
      if (!wantsScan) Nil
      else
        resolvedDependencyJars.flatMap { jarPath =>
          annotationProcessorClasses(jarPath).filter(_.nonEmpty) match {
            case Some(classes) =>
              logger.info(s"[${crossName.value}] auto-discovered annotation processor JAR: $jarPath — classes: ${classes.mkString(", ")}")
              Some(jarPath)
            case None => None
          }
        }

    if (wantsScan && explicitJars.isEmpty && scannedJars.isEmpty)
      sys.error(
        s"project ${crossName.value}: scanForAnnotationProcessors: true was set but no annotation processor JARs were found in dependencies and annotationProcessors is empty"
      )

    val processorJars: List[Path] = (scannedJars ++ explicitJars).distinct
    AnnotationProcessorResult(
      processorJars = processorJars,
      genSourcesDir = genSourcesDir,
      aFlags = SortedMap.from(java.annotationProcessorOptions.value)
    )
  }

  /** When `jarPath` declares JSR 269 annotation processors via ServiceLoader registration (i.e. has a `META-INF/services/javax.annotation.processing.Processor`
    * entry), returns the parsed list of fully-qualified class names. `None` means the jar has no service file. `Some(Nil)` means the file exists but is empty /
    * comment-only.
    */
  def annotationProcessorClasses(jarPath: Path): Option[List[String]] =
    if (!java.nio.file.Files.isRegularFile(jarPath)) None
    else {
      val zip = new java.util.zip.ZipFile(jarPath.toFile)
      try {
        val entry = zip.getEntry("META-INF/services/javax.annotation.processing.Processor")
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

  /** True iff the user has `-proc:none` in `java.options` — the escape hatch that disables all auto-wiring. */
  def userOptsOut(java: model.Java): Boolean =
    java.options.values.exists(_.render.contains("-proc:none"))
}
