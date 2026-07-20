package bleep.analysis

import bleep.bsp.ProjectLock
import bleep.model
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.concurrent.duration._

/** Regression tests for how `ProjectLock` keys its within-JVM bookkeeping.
  *
  * The bug: `states`/`locks` were keyed by `CrossProjectName` while the cross-process file lock was keyed by `<targetDir>/.bleep-lock`. A `normal`-variant and
  * a `bsp`-variant compile of the same project write disjoint directories and take disjoint file locks, but contended on a single `ProjectState` — so an IDE
  * compile could stall a CLI compile of the same project for the full 5 minute timeout.
  */
class ProjectLockKeyingTest extends AnyFunSuite with Matchers {

  private val project = model.CrossProjectName(model.ProjectName("demo"), crossId = None)

  private def outputDirIn(root: Path, variant: String): Path = {
    val dir = root.resolve(variant).resolve("classes")
    Files.createDirectories(dir)
    dir
  }

  private def exclusive(outputDir: Path, timeout: FiniteDuration) =
    ProjectLock.acquire(
      project = project,
      outputDir = outputDir,
      mode = ProjectLock.LockMode.Exclusive,
      timeout = timeout,
      onContention = () => ()
    )

  private def withTempRoot[A](f: Path => A): A = {
    val root = Files.createTempDirectory("project-lock-keying")
    try f(root)
    finally bleep.internal.FileUtils.deleteDirectory(root)
  }

  test("same project in two build variants does not contend") {
    withTempRoot { root =>
      val normal = outputDirIn(root, "normal")
      val bsp = outputDirIn(root, "bsp")

      // Holding the exclusive lock for the `normal` variant must not block the `bsp` variant.
      // Before the fix this timed out, because both mapped to one CrossProjectName-keyed state.
      val bothHeld = exclusive(normal, 5.seconds)
        .use { _ =>
          exclusive(bsp, 5.seconds).use(_ => IO.pure(true))
        }
        .unsafeRunSync()

      bothHeld shouldBe true
    }
  }

  test("same project in the same variant still contends") {
    withTempRoot { root =>
      val normal = outputDirIn(root, "normal")

      // The lock must still do its job: one output directory, one exclusive holder.
      val outcome = exclusive(normal, 300.millis)
        .use { _ =>
          exclusive(normal, 300.millis).use(_ => IO.unit)
        }
        .attempt
        .unsafeRunSync()

      outcome.left.map(_.getClass) shouldBe Left(classOf[ProjectLock.LockTimeoutException])
    }
  }
}
