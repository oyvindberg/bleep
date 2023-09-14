package bleep

import bleep.CoursierResolver.{Cached, Direct, Params}
import bleep.internal.FileUtils
import coursier.Fetch
import coursier.error.CoursierError
import io.circe.parser.decode
import io.circe.syntax.EncoderOps

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import scala.collection.immutable.SortedSet
import scala.collection.mutable

// this is a performance cache, the real cache is the coursier folder
class TestResolver(underlying: CoursierResolver, inMemoryCache: mutable.Map[CoursierResolver.Cached.Request, CoursierResolver.Result])
    extends CoursierResolver {
  override val params = underlying.params

  override def withParams(newParams: Params): CoursierResolver =
    new TestResolver(underlying.withParams(newParams), inMemoryCache)

  override def resolve(
      deps: SortedSet[model.Dep],
      versionCombo: model.VersionCombo,
      libraryVersionSchemes: SortedSet[model.LibraryVersionScheme]
  ): Either[CoursierError, CoursierResolver.Result] =
    if (deps.exists(_.version.endsWith("-SNAPSHOT"))) underlying.resolve(deps, versionCombo, libraryVersionSchemes)
    else {
      val request = CoursierResolver.Cached.Request(deps, underlying.params, versionCombo, libraryVersionSchemes)

      inMemoryCache.get(request) match {
        case Some(value) => Right(value)
        case None =>
          underlying.resolve(deps, versionCombo, libraryVersionSchemes).map {
            case changingResult if changingResult.fullDetailedArtifacts.exists { case (_, _, artifact, _) => artifact.changing } =>
              sys.error("tests are not allowed to use changing artifacts as it will be too slow")
            case result =>
              inMemoryCache(request) = result
              result
          }
      }
    }

  override def direct(
      deps: SortedSet[model.Dep],
      versionCombo: model.VersionCombo,
      libraryVersionSchemes: SortedSet[model.LibraryVersionScheme]
  ): Either[CoursierError, Fetch.Result] =
    underlying.direct(deps, versionCombo, libraryVersionSchemes)
}

object TestResolver {
  case class NoDownloadInCI(params: CoursierResolver.Params) extends CoursierResolver {
    override def withParams(newParams: Params): CoursierResolver = NoDownloadInCI(newParams)

    private def complain =
      new CoursierError("tried to download dependencies in CI. This is because the resolve cache is not filled properly") {}

    override def resolve(
        deps: SortedSet[model.Dep],
        versionCombo: model.VersionCombo,
        libraryVersionSchemes: SortedSet[model.LibraryVersionScheme]
    ): Either[CoursierError, CoursierResolver.Result] =
      Left(complain)

    override def direct(
        deps: SortedSet[model.Dep],
        versionCombo: model.VersionCombo,
        libraryVersionSchemes: SortedSet[model.LibraryVersionScheme]
    ): Either[CoursierError, Fetch.Result] =
      Left(complain)
  }

  def withFactory[T](isCi: Boolean, cacheFolder: Path, replacements: model.Replacements)(f: CoursierResolver.Factory => T): T = {
    val cacheFile = cacheFolder / "resolve-cache.json.gz"

    val inMemoryCache: mutable.Map[Cached.Request, CoursierResolver.Result] = {
      val existing =
        if (FileUtils.exists(cacheFile)) {
          val str = new String(FileUtils.readGzippedBytes(cacheFile), StandardCharsets.UTF_8)

          decode[Vector[(CoursierResolver.Cached.Request, CoursierResolver.Result)]](str)
            .orThrowWithError("couldn't decode json")
            .map { case (req, res) =>
              val newRes =
                res.copy(fullDetailedArtifacts = res.fullDetailedArtifacts.map { case (dep, p, a, of) => (dep, p, a, of.map(replacements.fill.file)) })
              (req, newRes)
            }
        } else Vector.empty
      mutable.Map.from(existing)
    }

    val factory: CoursierResolver.Factory = { (pre, _, buildFile) =>
      lazy val replacements = model.Replacements.paths(pre.buildPaths.buildDir)

      val resolvers = buildFile.resolvers.values.map {
        case model.Repository.Maven(name, uri)        => model.Repository.Maven(name, replacements.fill.uri(uri))
        case model.Repository.MavenFolder(name, path) => model.Repository.MavenFolder(name, replacements.fill.path(path))
        case model.Repository.Ivy(name, uri)          => model.Repository.Ivy(name, replacements.fill.uri(uri))
      }

      val params = CoursierResolver.Params(
        overrideCacheFolder = None,
        downloadSources = false,
        authentications = None,
        repos = resolvers
      )
      val underlying = if (isCi) NoDownloadInCI(params) else new CoursierResolver.Direct(pre.logger, pre.cacheLogger, params)
      val cached = new TestResolver(underlying, inMemoryCache)
      new CoursierResolver.TemplatedVersions(cached, maybeWantedBleepVersion = None)
    }

    try f(factory)
    finally
      if (!isCi) {
        val vector = inMemoryCache.toVector.map { case (req, res) =>
          val trimmedRes = res.copy(fullDetailedArtifacts = res.fullDetailedArtifacts.map { case (dep, p, a, of) =>
            val slimmedArtifact = a.withExtra(Map.empty).withChecksumUrls(Map.empty)
            val templatedFile = of.map(replacements.templatize.file)
            (dep, p, slimmedArtifact, templatedFile)
          })
          (req, trimmedRes)
        }
        FileUtils.writeGzippedBytes(cacheFile, vector.asJson.noSpaces.getBytes(StandardCharsets.UTF_8))
      }
  }
}
