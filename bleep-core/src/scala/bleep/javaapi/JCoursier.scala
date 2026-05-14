package bleep.javaapi

import bleep.{model, BleepException, Started}

import java.nio.file.Path
import scala.collection.immutable.SortedSet
import scala.jdk.CollectionConverters.*

/** Bridge for {@code bleepscript.Coursier} — fetches a classpath through {@code started.resolver}. */
object JCoursier {

  def fetchClasspath(
      jstarted: bleepscript.Started,
      coordinates: String
  ): java.util.List[Path] = {
    val started: Started = jstarted match {
      case js: JStarted => js.underlying
      case _            => throw new RuntimeException(s"Unknown Started impl: ${jstarted.getClass}")
    }

    val dep = model.Dep.parse(coordinates) match {
      case Right(d)  => d
      case Left(err) => throw new IllegalArgumentException(s"Invalid dependency coordinates '$coordinates': $err")
    }

    // For a Java dependency coordinate (single colon), VersionCombo.Java suffices. For Scala-cross
    // (double colon) the user-supplied coords already encode the cross suffix.
    val versionCombo = model.VersionCombo.Java
    val result = started.resolver.resolve(
      SortedSet(dep),
      versionCombo,
      libraryVersionSchemes = SortedSet.empty[model.LibraryVersionScheme],
      ignoreEvictionErrors = model.IgnoreEvictionErrors.No
    )

    result match {
      case Right(res) => res.jars.asJava
      case Left(err)  => throw new BleepException.ResolveError(err, coordinates)
    }
  }
}
