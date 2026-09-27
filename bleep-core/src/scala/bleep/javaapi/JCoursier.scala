package bleep.javaapi

import bleep.{model, BleepException, Started}

import java.nio.file.Path
import scala.collection.immutable.SortedSet
import scala.jdk.CollectionConverters.*

/** Bridge for {@code bleepscript.Coursier} — fetches a classpath through {@code started.resolver}. */
object JCoursier {

  def fetchClasspath(jstarted: bleepscript.Started, coordinates: String): java.util.List[Path] =
    resolve(jstarted, coordinates, downloadSources = false).jars.asJava

  /** The sources jars of `coordinates` and its transitive closure. */
  def fetchSources(jstarted: bleepscript.Started, coordinates: String): java.util.List[Path] =
    resolve(jstarted, coordinates, downloadSources = true).fullDetailedArtifacts
      .collect { case (_, publication, _, Some(file)) if publication.classifier == coursier.core.Classifier.sources => file.toPath }
      .distinct
      .asJava

  private def resolve(jstarted: bleepscript.Started, coordinates: String, downloadSources: Boolean): bleep.CoursierResolver.Result = {
    val started: Started = jstarted match {
      case js: JStarted => js.underlying
      case _            => throw new RuntimeException(s"Unknown Started impl: ${jstarted.getClass}")
    }

    val dep = model.Dep.parse(coordinates) match {
      case Right(d)  => d
      case Left(err) => throw new IllegalArgumentException(s"Invalid dependency coordinates '$coordinates': $err")
    }

    val resolver = if (downloadSources) started.resolver.withParams(started.resolver.params.copy(downloadSources = true)) else started.resolver
    // For a Java dependency coordinate (single colon), VersionCombo.Java suffices. For Scala-cross
    // (double colon) the user-supplied coords already encode the cross suffix.
    resolver.resolve(
      SortedSet(dep),
      model.VersionCombo.Java,
      libraryVersionSchemes = SortedSet.empty[model.LibraryVersionScheme],
      ignoreEvictionErrors = model.IgnoreEvictionErrors.No
    ) match {
      case Right(res) => res
      case Left(err)  => throw new BleepException.ResolveError(err, coordinates)
    }
  }
}
