package bleep

import coursier.LocalRepositories
import coursier.cache.{ArchiveCache, CacheLogger, FileCache}
import coursier.core.Authentication
import coursier.util.{Artifact, Task}
import io.circe.parser.decode
import io.circe.Decoder

import java.io.File
import java.net.URI
import java.nio.file.{Files, Path, StandardCopyOption}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

/** Fetch a snapshot bleep from the CI run that produced it.
  *
  * A released version is a URL: `releases/download/v$version/bleep-$arch.tar.gz`, and [[FetchBleepRelease]] just builds it. A snapshot has no release, only the
  * artifacts of the workflow run for its commit — which is why this exists, and why it is more than string concatenation: the artifacts have to be looked up by
  * commit, and downloading them needs a token.
  *
  * ==Why a token==
  *
  * Measured against the real API rather than assumed: listing a public repository's artifacts is anonymous (HTTP 200), downloading one is not (HTTP 401). There
  * is no anonymous path, so a snapshot cannot be made to "just work" the way a release does. Release assets would have been anonymous, which is the trade this
  * design accepted in exchange for reaching branch builds and not creating a GitHub release per commit.
  *
  * ==Why the jars too==
  *
  * A bleep binary asks Coursier for `build.bleep:bleep-bsp` at the version baked into it, and client, server and test-runner must all be that same version. So
  * fetching the binary alone produces something that starts and then fails to resolve its own server. The `bleep-jars` artifact from the same run is unpacked
  * into `~/.ivy2/local`, which [[bleep.constants.DefaultRepos]] searches ahead of Maven Central.
  */
object FetchBleepSnapshot {

  val Owner = "oyvindberg"
  val Repo = "bleep"

  /** Same derivation as `bleep.constants.ivy2Path`, which lives in bleep-core and is therefore not visible from here. Both read it off coursier so the
    * directory bleep publishes to and the directory it resolves from cannot drift apart.
    */
  val defaultIvy2Local: Path = Path.of(URI.create(LocalRepositories.ivy2Local.pattern.chunks.head.string))

  /** dynver shapes a snapshot as `<tag>+<distance>-<sha>[+<dirty timestamp>][-SNAPSHOT]`, e.g. `1.0.0-M10+83-fbe31c09-SNAPSHOT`. The sha is what identifies the
    * run; everything after it is noise for this purpose.
    */
  private val ShaFromVersion = """^[^+]+\+\d+-([0-9a-f]{7,40})\b.*$""".r

  def shaOf(version: model.BleepVersion): Option[String] =
    version.value match {
      case ShaFromVersion(sha) => Some(sha)
      case _                   => None
    }

  /** `GITHUB_TOKEN` and `GH_TOKEN` are what CI and most tooling set; `gh auth token` is what a developer already has. Shelling out last, because it is the only
    * one that costs a process.
    */
  def findToken(): Option[String] =
    sys.env
      .get("GITHUB_TOKEN")
      .filter(_.nonEmpty)
      .orElse(sys.env.get("GH_TOKEN").filter(_.nonEmpty))
      .orElse {
        scala.util
          .Try {
            val out = scala.sys.process.Process(List("gh", "auth", "token")).!!.trim
            if (out.isEmpty) None else Some(out)
          }
          .toOption
          .flatten
      }

  private case class Commit(sha: String, parents: List[Parent])
  private case class Parent(sha: String)
  private case class Run(id: Long, status: String, conclusion: Option[String])
  private case class RunsPage(workflow_runs: List[Run])
  private case class GhArtifact(id: Long, name: String, expired: Boolean)
  private case class ArtifactPage(artifacts: List[GhArtifact])

  private implicit val parentDecoder: Decoder[Parent] = Decoder.forProduct1("sha")(Parent.apply)
  private implicit val commitDecoder: Decoder[Commit] = Decoder.forProduct2("sha", "parents")(Commit.apply)
  private implicit val runDecoder: Decoder[Run] = Decoder.forProduct3("id", "status", "conclusion")(Run.apply)
  private implicit val runsDecoder: Decoder[RunsPage] = Decoder.forProduct1("workflow_runs")(RunsPage.apply)
  private implicit val artifactDecoder: Decoder[GhArtifact] = Decoder.forProduct3("id", "name", "expired")(GhArtifact.apply)
  private implicit val pageDecoder: Decoder[ArtifactPage] = Decoder.forProduct1("artifacts")(ArtifactPage.apply)

  /** GitHub's own username for a token used as a password. Any non-empty user works; this one is what GitHub documents. */
  private def auth(token: String) = Authentication("x-access-token", token)

  private def api(path: String) = s"https://api.github.com/repos/$Owner/$Repo/$path"

  private def downloadUrl(id: Long) =
    s"https://api.github.com/repos/$Owner/$Repo/actions/artifacts/$id/zip"

  /** The binary artifact name per platform, matching `matrix.artifact_name` in build.yml. */
  def artifactNameFor(osArch: OsArch.HasNativeImage): Either[String, String] =
    osArch match {
      case OsArch.MacosArm64(_) => Right("bleep-arm64-apple-darwin")
      case OsArch.MacosAmd64    => Right("bleep-x86_64-apple-darwin")
      case OsArch.LinuxAmd64    => Right("bleep-x86_64-pc-linux")
      case OsArch.LinuxArm64    => Right("bleep-arm64-pc-linux")
      case OsArch.WindowsAmd64  => Right("bleep-x86_64-pc-win32")
    }

  private def fetchJson(url: String, token: String, cacheLogger: CacheLogger, ec: ExecutionContext): Either[String, String] = {
    val artifact = Artifact(url).withAuthentication(Some(auth(token)))
    // `ttl = 0` so a snapshot published minutes ago is not hidden behind a cached listing from before it existed.
    val cache = FileCache[Task]().withLogger(cacheLogger).withTtl(Duration.Zero)
    Await.result(cache.file(artifact).run.value(ec), Duration.Inf) match {
      case Left(err)   => Left(s"could not list artifacts: ${err.describe}")
      case Right(file) => Right(Files.readString(file.toPath))
    }
  }

  private def get[A: Decoder](url: String, what: String, token: String, cacheLogger: CacheLogger, ec: ExecutionContext): Either[String, A] =
    fetchJson(url, token, cacheLogger, ec).flatMap(body => decode[A](body).left.map(e => s"could not read $what: ${e.getMessage}"))

  /** Commit -> run -> that run's artifacts.
    *
    * Not `?name=<artifact>` filtered by commit, which was the obvious shape and is quietly wrong: that endpoint pages, and it already returns a full page of
    * 100 for a single name here, so a snapshot whose run has fallen off page one reports "not found" while its artifact is sitting there unexpired. Going via
    * the run is exact, and one listing covers both the binary and the jars.
    *
    * The commit lookup exists because `?head_sha=` matches only a full 40-character sha, while the version carries eight.
    */
  private def artifactsForCommit(sha: String, token: String, cacheLogger: CacheLogger, ec: ExecutionContext): Either[String, List[GhArtifact]] =
    for {
      commit <- get[Commit](api(s"commits/$sha"), s"commit $sha", token, cacheLogger, ec)
      run <- runFor(commit, sha, token, cacheLogger, ec)
      page <- get[ArtifactPage](api(s"actions/runs/${run.id}/artifacts?per_page=100"), s"the artifacts of run ${run.id}", token, cacheLogger, ec)
      _ <-
        if (page.artifacts.nonEmpty || run.status == "completed") Right(())
        else Left(s"the CI run for commit $sha is still ${run.status} and has not uploaded its artifacts yet")
    } yield page.artifacts

  /** The run whose `head_sha` is this commit — or, for a build of a pull request, the run for the commit that was merged.
    *
    * `actions/checkout` checks out `refs/pull/N/merge` for a pull_request event, so the version baked into that build names an ephemeral merge commit while the
    * run is recorded against the branch head. Looking up the merge commit therefore finds the commit and then zero runs. Verified against a real PR build:
    * `0.0.0+1-53645ab8-SNAPSHOT`, where 53645ab8 is `Merge fbe31c09 into 082c3840` and only fbe31c09 has a run.
    *
    * GitHub writes those parents as [base, head], so the second is the commit someone actually built. Reached only when the direct lookup finds nothing, so a
    * push build — every master build, and every release — takes the first branch and never depends on this.
    */
  private def runFor(commit: Commit, sha: String, token: String, cacheLogger: CacheLogger, ec: ExecutionContext): Either[String, Run] = {
    def runsFor(candidate: String) =
      get[RunsPage](api(s"actions/runs?head_sha=$candidate&per_page=20"), s"the runs for $candidate", token, cacheLogger, ec).map(_.workflow_runs)

    runsFor(commit.sha).flatMap {
      case run :: _ => Right(run)
      case Nil      =>
        commit.parents match {
          case _ :: merged :: Nil =>
            runsFor(merged.sha).flatMap {
              case run :: _ => Right(run)
              case Nil      => Left(s"no CI run found for commit $sha, nor for ${merged.sha.take(8)} which it merged")
            }
          case _ => Left(s"no CI run found for commit $sha")
        }
    }
  }

  private def pick(artifacts: List[GhArtifact], name: String, sha: String): Either[String, GhArtifact] =
    artifacts.find(_.name == name) match {
      case Some(found) if !found.expired => Right(found)
      // Retention is finite by design, so this is a normal outcome for an old snapshot rather than a failure to explain away.
      case Some(_) => Left(s"the '$name' artifact for commit $sha has expired. CI keeps these for 7 days; rebuild that commit or use a released version")
      case None    => Left(s"the CI run for commit $sha has no '$name' artifact — it may have failed before uploading")
    }

  private def download(art: GhArtifact, token: String, cacheLogger: CacheLogger, ec: ExecutionContext): Either[String, File] = {
    // The zip endpoint 302s to a signed blob URL that rejects credentials, so they must not survive the redirect — which is coursier's default.
    val artifact = Artifact(downloadUrl(art.id)).withAuthentication(Some(auth(token)))
    val cache = ArchiveCache[Task]().withCache(FileCache().withLogger(cacheLogger))
    Await.result(cache.get(artifact).value(ec), Duration.Inf).left.map(err => s"could not download '${art.name}': ${err.describe}")
  }

  /** Unpack the jars beside the binary, into the ivy directory the binary will search. Copying rather than moving, and skipping what is already there, so a
    * second run of the same snapshot is a no-op instead of a re-download.
    */
  private def installJars(extracted: File, ivy2Local: Path): Either[String, Int] = {
    val src = new File(extracted, "build.bleep")
    if (!src.isDirectory) Left(s"the bleep-jars artifact has no build.bleep directory (found: ${Option(extracted.list()).fold("nothing")(_.mkString(", "))})")
    else {
      var copied = 0
      def go(from: File, to: Path): Unit =
        if (from.isDirectory) {
          Files.createDirectories(to)
          Option(from.listFiles()).toList.flatten.foreach(child => go(child, to.resolve(child.getName)))
        } else if (!Files.exists(to)) {
          Files.createDirectories(to.getParent)
          Files.copy(from.toPath, to, StandardCopyOption.REPLACE_EXISTING)
          copied += 1
        }
      go(src, ivy2Local.resolve("build.bleep"))
      Right(copied)
    }
  }

  /** @return
    *   the path of the executable, with its server jars installed
    */
  def apply(
      wanted: model.BleepVersion,
      cacheLogger: CacheLogger,
      ec: ExecutionContext,
      osArch: OsArch.HasNativeImage,
      ivy2Local: Path
  ): Either[BleepException, Path] = {
    def fail(msg: String) = Left(new BleepException.Text(s"couldn't fetch bleep ${wanted.value}: $msg"))

    shaOf(wanted) match {
      case None      => fail(s"no commit sha in the version, so there is no CI run to fetch it from")
      case Some(sha) =>
        findToken() match {
          case None =>
            fail(
              "a snapshot lives in the CI artifacts of its commit, and GitHub requires a token to download those even for a public repository. " +
                "Set GITHUB_TOKEN or GH_TOKEN, or run `gh auth login`"
            )
          case Some(token) =>
            val result =
              for {
                binaryName <- artifactNameFor(osArch)
                artifacts <- artifactsForCommit(sha, token, cacheLogger, ec)
                binaryArt <- pick(artifacts, binaryName, sha)
                jarsArt <- pick(artifacts, "bleep-jars", sha)
                binaryDir <- download(binaryArt, token, cacheLogger, ec)
                jarsDir <- download(jarsArt, token, cacheLogger, ec)
                _ <- installJars(jarsDir, ivy2Local)
                exe <- FetchBleepRelease.findExecutable(binaryDir)
              } yield exe.toPath

            result match {
              case Left(msg)   => fail(msg)
              case Right(path) =>
                path.toFile.setExecutable(true)
                Right(path)
            }
        }
    }
  }
}
