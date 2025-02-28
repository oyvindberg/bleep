package bleep
package testing

import coursier.paths.CoursierPaths
import org.scalactic.TripleEqualsSupport
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import ryddig.{LogLevel, LogPatterns, Logger, Loggers}

import java.nio.file.{Files, Path, Paths}
import java.time.Instant
import scala.util.Properties

trait SnapshotTest extends AnyFunSuite with TripleEqualsSupport {
  val logger0 = Loggers.stdout(LogPatterns.interface(Some(Instant.now), noColor = false), disableProgress = true).acquire().value.withMinLogLevel(LogLevel.info)

  val isCi: Boolean =
    sys.env.contains("BUILD_NUMBER") || sys.env.contains("CI") // from sbt

  val outFolder: Path =
    Paths.get("snapshot-tests").toAbsolutePath

  val absolutePaths: model.Replacements =
    model.Replacements.ofReplacements(
      List(
        (CoursierPaths.cacheDirectory().toString, "<COURSIER>"),
        (CoursierPaths.archiveCacheDirectory().toString, "<COURSIER_ARC>"),
        (System.getProperty("user.dir"), "<BLEEP_GIT>"),
        (System.getProperty("user.home"), "<HOME>"),
        (System.getProperty("java.home"), "<JAVA_HOME>"),
        ("darwin", "<MASKED_OS>"),
        ("linux", "<MASKED_OS>"),
        ("windows", "<MASKED_OS>"),
        ("x64", "<MASKED_ARCHITECTURE>"),
        ("amd64", "<MASKED_ARCHITECTURE>"),
        ("arm64", "<MASKED_ARCHITECTURE>")
      )
    )

  def writeAndCompare(in: Path, fileMap: Map[Path, String], logger: Logger): Assertion = {
    FileSync.syncPaths(in, fileMap, deleteUnknowns = FileSync.DeleteUnknowns.Yes(maxDepth = None), soft = true).discard()

    var from = in
    while (!Files.isDirectory(from))
      from = from.getParent

    GitLock.synchronized {
      cli("git add", from, List("/usr/bin/git", "add", in.toString), logger, out = cli.Out.ViaLogger(logger), env = List(("PATH", sys.env("PATH")))).discard()
    }

    if (Properties.isWin) succeed // let's deal with this later
    else if (isCi) {
      GitLock.synchronized {
        cli(
          "git diff",
          from,
          List("/usr/bin/git", "diff", "--exit-code", "HEAD", in.toString),
          logger,
          out = cli.Out.ViaLogger(logger),
          env = List(("PATH", sys.env("PATH")))
        ).discard()
      }
      succeed
    } else succeed
  }
}

private object GitLock
