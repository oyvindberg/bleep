package bleep

import bleep.commands.PublishVersion
import bleep.internal.gitOutput
import bleep.model.StampKind

import java.nio.charset.StandardCharsets
import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}
import java.security.MessageDigest
import scala.util.control.NonFatal

/** Computes a project's `stamp:` values and writes them to the file [[StampFile]] names.
  *
  * ==Where it runs==
  *
  * In the BSP server's compile step, for every project a request compiles — including projects that are already up to date, and projects just restored from the
  * remote cache. Every link, test fork and run depends on its project's compile task, so the stamp exists before anything that could read it. And it is the one
  * step every client goes through: `bleep compile`/`test`/`run`/`link`/`ci`, a `--watch` cycle, an IDE's `buildTarget/compile`, the MCP server.
  *
  * The one exception is publishing, which re-stamps on the client just before packaging with the version it publishes under ([[materialize]]) — the only party
  * that knows it is the client.
  *
  * ==Always recomputed==
  *
  * Never checked for staleness. A stamp is in no cache key ([[Usage.Runtime]]), so recomputing it invalidates nothing, and the one expensive-to-watch input —
  * git state, including an uncommitted working tree — is exactly what makes a staleness check wrong. Recomputing costs one `git describe` per request; getting
  * it wrong is the bug stamps exist to remove. The file is only rewritten when its content changes.
  *
  * A request in which no project declares a stamp does no work here at all: no git, no digests.
  */
object Stamps {

  /** Stamp values for one pass over the build — one BSP request, or one publish.
    *
    * Each value is computed at most once, lazily, and only if some project asks for it: a build stamping only `git-sha` never runs `git describe`, and one
    * stamping neither digest never pays for [[ProjectDigest.computeAll]]. Compile tasks call [[write]] concurrently; Scala's lazy vals make each value's
    * computation happen once.
    *
    * @param publishingAs
    *   the version a publish is publishing under, which [[StampKind.Dynver]] then carries verbatim — so the published coordinate and the value inside the jar
    *   cannot disagree, whether it came from `--version` or from git. `None` for every pass that is not a publish, where it is derived the way a publish would
    *   derive it.
    */
  final class Pass private[Stamps] (started: Started, publishingAs: Option[String]) {
    private val buildDir = started.buildPaths.buildDir

    private lazy val dynver: String =
      publishingAs.getOrElse(PublishVersion.resolve(PublishVersion.Dynver, buildDir, assertRelease = false).orThrow)

    private lazy val gitSha: String =
      try gitOutput(buildDir, List("git", "rev-parse", "HEAD")).trim
      catch {
        case NonFatal(th) =>
          throw new BleepException.Cause(th, s"a project declares `stamp: [${StampKind.GitSha.value}]`, but $buildDir has no git commit to name")
      }

    private lazy val projectDigests: Map[model.CrossProjectName, String] = ProjectDigest.computeAll(started.build, started.buildPaths)

    // Over every project's portable digest, never over anything path-dependent. `BspBuildData.BuildId` would have been free, but it hashes absolute paths (it is
    // a machine-local daemon cache key), so two machines building one commit would stamp different values and the jars would stop being reproducible.
    private lazy val buildDigest: String = {
      val md = MessageDigest.getInstance("SHA-256")
      projectDigests.toList.sortBy(_._1).foreach { case (crossName, digest) =>
        md.update(crossName.value.getBytes(StandardCharsets.UTF_8))
        md.update(digest.getBytes(StandardCharsets.UTF_8))
      }
      Checksums.byteArrayToHexString(md.digest())
    }

    /** Write `crossName`'s stamps file, if it declared any stamps. A no-op otherwise, and a no-op when the content is already what it would write. */
    def write(crossName: model.CrossProjectName): Unit = {
      val kinds = started.build.explodedProjects(crossName).stamp.values.toList.sorted
      if (kinds.nonEmpty) {
        val lines = kinds.map { kind =>
          val value = kind match {
            case StampKind.Dynver        => dynver
            case StampKind.GitSha        => gitSha
            case StampKind.ProjectDigest => projectDigests(crossName)
            case StampKind.BuildDigest   => buildDigest
          }
          s"${kind.value}=${encodeValue(crossName, kind, value)}\n"
        }
        writeIfChanged(fileFor(started, crossName), lines.mkString.getBytes(StandardCharsets.US_ASCII))
      }
    }
  }

  def pass(started: Started, publishingAs: Option[String]): Pass = new Pass(started, publishingAs)

  /** Write the stamps file for every project in the build that declared any. What a publish uses to re-stamp with the version it publishes under. */
  def materialize(started: Started, publishingAs: Option[String]): Unit = {
    val p = pass(started, publishingAs)
    started.build.explodedProjects.keys.foreach(p.write)
  }

  /** Where `crossName`'s stamps are written: inside the stamps root the path model placed on its runtime classpath. */
  def fileFor(started: Started, crossName: model.CrossProjectName): Path = {
    val root = started
      .projectPaths(crossName)
      .resourcesDirs
      .stamps
      .getOrElse(
        throw new BleepException.Text(s"${crossName.value} declares stamps, but its path model has no stamps directory: BuildPaths and Stamps disagree")
      )
    root.resolve(StampFile.resourcePath(crossName))
  }

  /** Atomic, write-if-changed, and safe with several writers at once.
    *
    * Not `FileUtils.writeBytesAtomic`, whose staging file has one fixed name: two writers — an IDE and the CLI compiling the same project in the same daemon —
    * would share it, and the loser's rename fails with the staging file already moved away, failing a compile task over a stamp. A unique staging file per
    * write makes the last rename win, and every writer at one git state writes the same bytes anyway.
    */
  private def writeIfChanged(file: Path, content: Array[Byte]): Unit =
    if (!(Files.exists(file) && java.util.Arrays.equals(Files.readAllBytes(file), content))) {
      Files.createDirectories(file.getParent)
      val staging = Files.createTempFile(file.getParent, file.getFileName.toString, ".tmp")
      try {
        Files.write(staging, content).discard()
        try Files.move(staging, file, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING).discard()
        catch { case _: AtomicMoveNotSupportedException => Files.move(staging, file, StandardCopyOption.REPLACE_EXISTING).discard() }
      } finally Files.deleteIfExists(staging).discard()
    }

  /** `value` exactly as `java.util.Properties.load` will read it back.
    *
    * Written by hand rather than with `Properties.store`, which prepends a `#<timestamp>` comment and would make the file differ on every build. So the
    * escaping is ours: backslash doubled, and anything a `.properties` reader would not return verbatim — control characters, non-ASCII (`load` reads
    * ISO-8859-1), a leading space (`load` strips it) — throws. Every value bleep derives is safe by construction; the check exists for `--version`, which the
    * user typed.
    */
  private def encodeValue(owner: model.CrossProjectName, kind: StampKind, value: String): String = {
    if (value.isEmpty || value.head == ' ' || value.exists(c => c < 0x20 || c > 0x7e))
      throw new BleepException.Text(
        s"${owner.value}: stamp '${kind.value}' would be ${value.map(c => if (c < 0x20 || c > 0x7e) f"\\u${c.toInt}%04x" else c.toString).mkString("\"", "", "\"")}, " +
          "which a .properties reader would not get back verbatim"
      )
    value.replace("\\", "\\\\")
  }
}
