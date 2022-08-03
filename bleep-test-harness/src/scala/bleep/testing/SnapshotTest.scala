package bleep.testing

import bleep.{model, FileSync}
import coursier.jvm.JvmIndex
import coursier.paths.CoursierPaths
import org.scalactic.TripleEqualsSupport
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path, Paths}
import scala.util.Properties

trait SnapshotTest extends AnyFunSuite with TripleEqualsSupport {
  val enforceUpToDate: Boolean =
    sys.env.contains("BUILD_NUMBER") || sys.env.contains("CI") // from sbt

  val outFolder: Path =
    Paths.get("snapshot-tests").toAbsolutePath

  val absolutePaths: model.Replacements =
    model.Replacements.ofReplacements(
      List(
        Some((CoursierPaths.cacheDirectory().toString, "<COURSIER>")),
        Some((CoursierPaths.archiveCacheDirectory().toString, "<COURSIER_ARC>")),
        Some((JvmIndex.defaultOs(), "<OS>")),
        Some((JvmIndex.defaultArchitecture(), "<ARCHITECTURE>")),
        JvmIndex.defaultArchitecture() match {
          case "amd64" => Some(("x64", "<ARCHITECTURE>"))
          case _       => None
        },
        Some((System.getProperty("user.dir"), "<BLEEP_GIT>")),
        Some((System.getProperty("user.home"), "<HOME>"))
      ).flatten
    )

  def writeAndCompare(in: Path, fileMap: Map[Path, String]): Assertion =
    if (Properties.isWin) pending // let's deal with this later
    else {
      if (enforceUpToDate) {
        fileMap.foreach { case (path, contents) =>
          if (Files.exists(path)) {
            val existingContents = Files.readString(path)
            assert(existingContents === contents, path)
          } else {
            fail(s"Expected path $path to exist")
          }
        }
        succeed
      } else {
        FileSync.syncPaths(in, fileMap, deleteUnknowns = FileSync.DeleteUnknowns.Yes(maxDepth = None), soft = true)
        pending
      }
    }

  // copy/paste above. this won't throw a pending test result exception on success
  // also won't delete unknown files
  def writeAndCompareEarly(in: Path, fileMap: Map[Path, String]): Assertion =
    if (Properties.isWin) succeed // let's deal with this later
    else {
      if (enforceUpToDate) {
        fileMap.foreach { case (path, contents) =>
          if (Files.exists(path)) {
            val existingContents = Files.readString(path)
            assert(existingContents === contents)
          } else {
            fail(s"Expected path $path to exist")
          }
        }
        succeed
      } else {
        FileSync.syncPaths(in, fileMap, deleteUnknowns = FileSync.DeleteUnknowns.No, soft = true)
        succeed
      }
    }
}
