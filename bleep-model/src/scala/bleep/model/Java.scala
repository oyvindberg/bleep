package bleep.model

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Java compilation settings.
  *
  * @param options
  *   javac options
  * @param scanForAnnotationProcessors
  *   when set to true, bleep scans every resolved-`dependencies` jar for `META-INF/services/javax.annotation.processing.Processor` and adds matching jars to
  *   javac's `-processorpath`. Default off — explicit opt-in.
  * @param annotationProcessors
  *   processor-only deps. Resolved separately and added to `-processorpath` only — never on the runtime classpath. Composes with `scanForAnnotationProcessors`
  *   when both are set.
  * @param annotationProcessorOptions
  *   `-A<key>=<value>` flags passed globally to javac (any processor sees any `-A`).
  * @param ecjVersion
  *   optional ECJ (Eclipse Compiler for Java) version. If set, uses ECJ instead of javac.
  */
case class Java(
    options: Options,
    scanForAnnotationProcessors: Option[Boolean],
    annotationProcessors: JsonSet[Dep],
    annotationProcessorOptions: AnnotationProcessorOptions,
    ecjVersion: Option[VersionEcj] = None
) extends SetLike[Java] {
  override def intersect(other: Java): Java =
    Java(
      options = options.intersect(other.options),
      scanForAnnotationProcessors = if (scanForAnnotationProcessors == other.scanForAnnotationProcessors) scanForAnnotationProcessors else None,
      annotationProcessors = annotationProcessors.intersect(other.annotationProcessors),
      annotationProcessorOptions = annotationProcessorOptions.intersect(other.annotationProcessorOptions),
      ecjVersion = if (ecjVersion == other.ecjVersion) ecjVersion else None
    )

  override def removeAll(other: Java): Java =
    Java(
      options = options.removeAll(other.options),
      scanForAnnotationProcessors = if (scanForAnnotationProcessors == other.scanForAnnotationProcessors) None else scanForAnnotationProcessors,
      annotationProcessors = annotationProcessors.removeAll(other.annotationProcessors),
      annotationProcessorOptions = annotationProcessorOptions.removeAll(other.annotationProcessorOptions),
      ecjVersion = if (ecjVersion == other.ecjVersion) None else ecjVersion
    )

  override def union(other: Java): Java =
    Java(
      options = options.union(other.options),
      scanForAnnotationProcessors = scanForAnnotationProcessors.orElse(other.scanForAnnotationProcessors),
      annotationProcessors = annotationProcessors.union(other.annotationProcessors),
      annotationProcessorOptions = annotationProcessorOptions.union(other.annotationProcessorOptions),
      ecjVersion = ecjVersion.orElse(other.ecjVersion)
    )

  def isEmpty: Boolean =
    this match {
      case Java(options, scanForAnnotationProcessors, annotationProcessors, annotationProcessorOptions, ecjVersion) =>
        options.isEmpty &&
        scanForAnnotationProcessors.isEmpty &&
        annotationProcessors.isEmpty &&
        annotationProcessorOptions.isEmpty &&
        ecjVersion.isEmpty
    }
}

object Java {
  implicit val decodes: Decoder[Java] = deriveDecoder
  implicit val encodes: Encoder[Java] = deriveEncoder
}
