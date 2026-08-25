package bleep.bsp

import bleep.{model, ProjectPaths, RelPath}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path}
import java.time.Instant
import scala.collection.immutable.SortedSet
import scala.jdk.StreamConverters.*

/** Does a directory declared under `sourceGlobs` re-run the generator?
  *
  * It used to not. `sourceGlobs` reached the `--watch` file watcher and nothing else, so the watcher woke up on a schema edit, triggered a compile, and the
  * compile skipped the generator because the staleness check had never heard of `schema/`. The user saw a build run and produce stale code, which is worse than
  * seeing nothing happen at all.
  *
  * Timestamps here are set explicitly rather than by writing files in sequence — filesystem mtime granularity is one second on some filesystems, which would
  * make "the input is newer than the output" a coin flip.
  */
class SourcegenStalenessTest extends AnyFunSuite with Matchers {

  private def cpn(name: String): model.CrossProjectName =
    model.CrossProjectName(model.ProjectName(name), None)

  private def script(globs: String*): model.ScriptDef.Main =
    model.ScriptDef.Main(cpn("scripts"), "gen.Main", model.JsonSet(SortedSet.from(globs.map(RelPath.force))))

  private def emptyDirs: ProjectPaths.DirsByOrigin =
    ProjectPaths.DirsByOrigin(SortedSet.empty, Map.empty, Map.empty, None, Nil)

  /** ProjectPaths for a consumer: no sources of its own, one generated output directory per script. */
  private def consumerPaths(workspace: Path, name: String, forScript: model.ScriptDef.Main): ProjectPaths = {
    val dir = workspace.resolve(name)
    ProjectPaths(
      dir = dir,
      targetDir = workspace.resolve(".bleep").resolve(name),
      sourcesDirs = emptyDirs.copy(generated = Map(forScript -> workspace.resolve(".bleep/generated-sources").resolve(name))),
      resourcesDirs = emptyDirs,
      isTestProject = false
    )
  }

  private def writeAt(file: Path, content: String, at: Instant): Unit = {
    Files.createDirectories(file.getParent)
    Files.writeString(file, content)
    Files.setLastModifiedTime(file, FileTime.from(at)): Unit
  }

  private def deleteRecursively(path: Path): Unit =
    if (Files.exists(path)) {
      if (Files.isDirectory(path))
        scala.util.Using(Files.list(path))(_.toScala(List)).get.foreach(deleteRecursively)
      else path.toFile.setWritable(true): Unit
      Files.delete(path)
    }

  private def withWorkspace(f: Path => Unit): Unit = {
    val workspace = Files.createTempDirectory("bleep-sourcegen-staleness-")
    try f(workspace)
    finally deleteRecursively(workspace)
  }

  private val old = Instant.parse("2020-01-01T00:00:00Z")
  private val recent = Instant.parse("2030-01-01T00:00:00Z")

  test("a file under a declared sourceGlobs directory that is newer than the output makes the script stale") {
    withWorkspace { workspace =>
      val s = script("../schema")
      val paths = consumerPaths(workspace, "myapp", s)
      val scriptSrc = workspace.resolve("scripts/src/scala")

      writeAt(scriptSrc.resolve("Main.scala"), "object Main", old)
      writeAt(paths.sourcesDirs.generated(s).resolve(".sourcegen-stamp"), "", old.plusSeconds(60))
      writeAt(workspace.resolve("schema/users.sql"), "create table users (id int)", recent)

      SourceGenRunner.projectsNeedingRegeneration(
        s,
        Set(cpn("myapp")),
        Array(scriptSrc),
        _ => paths
      ) shouldBe Set(cpn("myapp"))
    }
  }

  test("the same layout without the sourceGlobs declaration is considered up to date") {
    withWorkspace { workspace =>
      val s = script()
      val paths = consumerPaths(workspace, "myapp", s)
      val scriptSrc = workspace.resolve("scripts/src/scala")

      writeAt(scriptSrc.resolve("Main.scala"), "object Main", old)
      writeAt(paths.sourcesDirs.generated(s).resolve(".sourcegen-stamp"), "", old.plusSeconds(60))
      writeAt(workspace.resolve("schema/users.sql"), "create table users (id int)", recent)

      SourceGenRunner.projectsNeedingRegeneration(s, Set(cpn("myapp")), Array(scriptSrc), _ => paths) shouldBe empty
    }
  }

  test("a declared directory older than the output leaves the script up to date") {
    withWorkspace { workspace =>
      val s = script("../schema")
      val paths = consumerPaths(workspace, "myapp", s)
      val scriptSrc = workspace.resolve("scripts/src/scala")

      writeAt(scriptSrc.resolve("Main.scala"), "object Main", old)
      writeAt(workspace.resolve("schema/users.sql"), "create table users (id int)", old)
      writeAt(paths.sourcesDirs.generated(s).resolve(".sourcegen-stamp"), "", recent)

      SourceGenRunner.projectsNeedingRegeneration(s, Set(cpn("myapp")), Array(scriptSrc), _ => paths) shouldBe empty
    }
  }

  test("a declared directory is enough on its own when the script project has no sources yet") {
    withWorkspace { workspace =>
      val s = script("../schema")
      val paths = consumerPaths(workspace, "myapp", s)

      writeAt(paths.sourcesDirs.generated(s).resolve(".sourcegen-stamp"), "", old)
      writeAt(workspace.resolve("schema/users.sql"), "create table users (id int)", recent)

      SourceGenRunner.projectsNeedingRegeneration(s, Set(cpn("myapp")), Array.empty, _ => paths) shouldBe Set(cpn("myapp"))
    }
  }

  test("no inputs anywhere means the script is skipped, not run") {
    withWorkspace { workspace =>
      val s = script("../schema")
      val paths = consumerPaths(workspace, "myapp", s)

      writeAt(paths.sourcesDirs.generated(s).resolve(".sourcegen-stamp"), "", old)

      SourceGenRunner.projectsNeedingRegeneration(s, Set(cpn("myapp")), Array.empty, _ => paths) shouldBe empty
    }
  }

  test("two consumers of one script are judged separately, because sourceGlobs resolves against each consumer's folder") {
    withWorkspace { workspace =>
      // Both consumers declare `../schema`, but the path resolves against their own folder, so they
      // are two different directories. Only the one whose directory changed is regenerated.
      val s = script("../schema")
      val stale = consumerPaths(workspace.resolve("stale-side"), "myapp", s)
      val fresh = consumerPaths(workspace.resolve("fresh-side"), "otherapp", s)
      val scriptSrc = workspace.resolve("scripts/src/scala")

      writeAt(scriptSrc.resolve("Main.scala"), "object Main", old)

      writeAt(stale.sourcesDirs.generated(s).resolve(".sourcegen-stamp"), "", old.plusSeconds(60))
      writeAt(workspace.resolve("stale-side/schema/users.sql"), "changed", recent)

      writeAt(fresh.sourcesDirs.generated(s).resolve(".sourcegen-stamp"), "", recent.plusSeconds(60))
      writeAt(workspace.resolve("fresh-side/schema/users.sql"), "unchanged", old)

      val pathsFor: model.CrossProjectName => ProjectPaths = {
        case n if n == cpn("myapp") => stale
        case _                      => fresh
      }

      SourceGenRunner.projectsNeedingRegeneration(
        s,
        Set(cpn("myapp"), cpn("otherapp")),
        Array(scriptSrc),
        pathsFor
      ) shouldBe Set(cpn("myapp"))
    }
  }
}
