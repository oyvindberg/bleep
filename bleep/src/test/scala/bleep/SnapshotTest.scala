package bleep

import bleep.internal.FileUtils
import bleep.internal.FileUtils.DeleteUnknowns
import org.scalactic.TripleEqualsSupport
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}
import scala.util.Properties

trait SnapshotTest extends AnyFunSuite with TripleEqualsSupport {
  val isCi: Boolean =
    sys.env.contains("BUILD_NUMBER") || sys.env.contains("CI") // from sbt

  def writeAndCompare(in: Path, fileMap: Map[RelPath, String]): Assertion =
    if (Properties.isWin) pending // let's deal with this later
    else {
      if (isCi) {
        fileMap.foreach { case (relPath, contents) =>
          val existingPath = in / relPath
          if (Files.exists(existingPath)) {
            val existingContents = Files.readString(existingPath)
            assert(existingContents === contents)
          } else {
            fail(s"Expected path $existingPath to exist")
          }
        }
        succeed
      } else {
        FileUtils.sync(in, fileMap, deleteUnknowns = DeleteUnknowns.Yes(maxDepth = None), soft = true)
        pending
      }
    }
}
