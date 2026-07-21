package bleep.analysis

import org.scalatest.concurrent.TimeLimits
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}

import java.nio.file.{Files, Path}
import scala.jdk.StreamConverters.*

/** Removing a Scala 3 source must remove its `.tasty`, not just its `.class`.
  *
  * Zinc tracks `.class` files as the products of a source and deletes them when the source goes away. It does not track the sibling `.tasty`, which for Scala 3
  * is what the compiler reads off the classpath to recover symbol definitions. An orphaned one keeps answering for a class whose source no longer exists.
  *
  * That turns an ordinary refactor — moving a file from one project to another — into a build that compiles against a definition that is no longer anywhere in
  * the source tree, reporting missing members on a class whose source plainly has them. Only `bleep clean` clears it.
  */
class OrphanedTastyTest extends AnyFunSuite with Matchers with TimeLimits {

  val mediumTimeout: Span = Span(120, Seconds)

  private def createTempWorkspace(prefix: String): Path = {
    val dir = Files.createTempDirectory(prefix)
    Files.createDirectories(dir.resolve("src"))
    dir
  }

  private def scalaLibraryClasspath(version: String): List[Path] =
    CompilerResolver.resolveScalaLibrary(version).toList

  private def deleteRecursively(path: Path): Unit =
    if (Files.exists(path)) {
      if (Files.isDirectory(path)) Files.list(path).toScala(List).foreach(deleteRecursively)
      Files.delete(path)
    }

  test("deleting a Scala source deletes its .tasty, not just its .class") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("orphan-tasty-")
      try {
        val goingAway = workspace.resolve("src/GoingAway.scala")
        val staying = workspace.resolve("src/Staying.scala")
        Files.writeString(goingAway, "package p\nobject GoingAway { def value: Int = 1 }\n")
        Files.writeString(staying, "package p\nobject Staying { def value: Int = 2 }\n")

        val config = BspTestHarness.ProjectConfig.scala(
          name = "scalaproject",
          sources = Set(workspace.resolve("src")),
          scalaVersion = "3.3.3",
          classpath = scalaLibraryClasspath("3.3.3"),
          isTest = false
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()
          val targetIds = client.buildTargets().targets.map(_.id)

          client.compile(targetIds).statusCode.value shouldBe 1

          val classes = BspTestBuild.classesDirFor(workspace, "scalaproject", isTest = false)
          val tasty = classes.resolve("p/GoingAway.tasty")
          val clazz = classes.resolve("p/GoingAway.class")
          Files.exists(clazz) shouldBe true
          Files.exists(tasty) shouldBe true

          // The refactor: the source goes away (moved elsewhere, or simply deleted).
          Files.delete(goingAway)
          client.compile(targetIds).statusCode.value shouldBe 1

          withClue("the .class was deleted, so Zinc did notice the source was removed: ") {
            Files.exists(clazz) shouldBe false
          }
          withClue("but the .tasty must go too — otherwise it keeps serving a stale API to every dependent: ") {
            Files.exists(tasty) shouldBe false
          }

          withClue("the surviving source must be untouched: ") {
            Files.exists(classes.resolve("p/Staying.tasty")) shouldBe true
          }
        }
      } finally deleteRecursively(workspace)
    }
  }
}
