package bleep.symbols

import coursierapi.{Cache, Dependency, Fetch, Module, Repository}

import java.nio.file.{Files, Path}
import java.util.zip.ZipFile
import scala.jdk.CollectionConverters.*

/** On-demand fetcher for `-sources.jar` artifacts.
  *
  * Bleep downloads sources eagerly for BSP but not for regular builds, so this fetches sources for a specific Maven coordinate just-in-time via Coursier, then
  * slices the requested file out of the zip.
  *
  * The coursier cache backs both bleep's resolver and our `Fetch.create()` here, so a sources jar fetched once is free the next time.
  */
object SourceFetcher {

  case class SourceResult(entryPath: String, startLine: Int, endLine: Int, lines: IndexedSeq[String])

  /** Coordinate format: `group:artifact:version`. Returns the path to the local sources jar, or None if the artifact has no sources published. */
  def fetchSourcesJar(coordinate: String, extraRepositories: Seq[Repository] = Seq.empty): Option[Path] = {
    val (group, artifact, version) = parseCoordinate(coordinate)
    try {
      val dep = Dependency.of(Module.of(group, artifact, java.util.Collections.emptyMap()), version).withTransitive(false)
      val fetch = Fetch
        .create()
        .addDependencies(dep)
        .withCache(Cache.create())
        .addClassifiers("sources")
        .withMainArtifacts(false)
      if (extraRepositories.nonEmpty) { val _ = fetch.addRepositories(extraRepositories*) }
      val files = fetch.fetch().asScala.toList
      files.headOption.map(_.toPath)
    } catch {
      case _: Throwable => None
    }
  }

  /** Slice a specific file out of a sources jar. `sourceFilePath` may be a suffix (e.g. `cats/Monad.scala`) — the first zip entry that ends with it wins.
    *
    * `endLine` may be `Int.MaxValue` to mean "to end of file".
    */
  def extractLines(jar: Path, sourceFilePath: String, startLine: Int, endLine: Int): Either[String, SourceResult] = {
    val normalizedSource = sourceFilePath.replace('\\', '/')
    val zip = new ZipFile(jar.toFile)
    try {
      val entry = zip
        .entries()
        .asScala
        .find(e => !e.isDirectory && normalizedSource.endsWith(e.getName))
      entry match {
        case None    => Left(s"Source file not found in JAR (looked for suffix of '$normalizedSource').")
        case Some(e) =>
          val in = zip.getInputStream(e)
          try {
            val allBytes = in.readAllBytes()
            val allLines = new String(allBytes, java.nio.charset.StandardCharsets.UTF_8).split('\n').toVector
            val extracted =
              if (endLine == Int.MaxValue) allLines.drop(startLine)
              else allLines.slice(startLine, math.min(endLine + 1, allLines.length))
            Right(SourceResult(e.getName, startLine, endLine, extracted))
          } finally in.close()
      }
    } finally zip.close()
  }

  /** Fetch + extract in one call. */
  def fetch(
      coordinate: String,
      sourceFilePath: String,
      startLine: Int,
      endLine: Int,
      extraRepositories: Seq[Repository] = Seq.empty
  ): Either[String, SourceResult] =
    fetchSourcesJar(coordinate, extraRepositories) match {
      case None      => Left(s"No sources JAR published for '$coordinate'.")
      case Some(jar) =>
        if (!Files.exists(jar)) Left(s"Sources JAR resolved but not present on disk: $jar")
        else extractLines(jar, sourceFilePath, startLine, endLine)
    }

  private def parseCoordinate(s: String): (String, String, String) =
    s.split(':') match {
      case Array(g, a, v) if g.nonEmpty && a.nonEmpty && v.nonEmpty => (g, a, v)
      case _                                                        =>
        throw new IllegalArgumentException(
          s"Invalid coordinate '$s'. Expected format: group:artifact:version (e.g. org.typelevel:cats-core_3:2.10.0)"
        )
    }
}
