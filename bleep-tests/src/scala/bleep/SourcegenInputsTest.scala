package bleep

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.collection.immutable.SortedSet
import scala.jdk.StreamConverters.*

/** "My generator reads a file — will bleep notice when it changes?"
  *
  * There are two ways to answer yes, and they must both stay yes. A directory declared under `sourceGlobs` on a `sourcegen:` entry is an input to the project
  * that declared it; the source and resource directories of the script project are inputs to every consumer of that script. These tests pin both against
  * [[ProjectDigest]], which is the remote-cache key and therefore the mechanism where being wrong is most expensive: a blind digest serves a cache hit built
  * from a schema that no longer exists.
  *
  * `sourceGlobs` used to reach exactly one call site — the `--watch` file watcher — so this file's first test was originally written the other way round, as a
  * characterization of the trap. It is now the regression test for the fix.
  */
class SourcegenInputsTest extends AnyFunSuite with Matchers {

  private def cpn(name: String): model.CrossProjectName =
    model.CrossProjectName(model.ProjectName(name), None)

  private def scriptDef(scriptProject: String, globs: String*): model.ScriptDef =
    model.ScriptDef.Main(
      cpn(scriptProject),
      "gen.Main",
      model.JsonSet(SortedSet.from(globs.map(RelPath.force)))
    )

  /** Every build here pins one, because [[ProjectDigest.computeAll]] refuses to digest a build whose toolchain is whatever `java` is on PATH. Which JDK does
    * not matter to these tests — they only ever compare two digests taken from builds that share it.
    */
  private val testJvm: model.Jvm = model.Jvm("temurin:17", None)

  private def makeBuild(projects: (String, model.Project)*): model.Build.Exploded =
    model.Build.Exploded(
      $version = model.BleepVersion("test"),
      explodedProjects = projects.map { case (name, p) => cpn(name) -> p }.toMap,
      resolvers = model.JsonList.empty,
      jvm = Some(testJvm),
      scripts = Map.empty,
      remoteCache = None
    )

  /** See the identical helper in [[ProjectDigestTest]] — Windows refuses to delete directories with open handles or read-only entries. */
  private def deleteRecursively(path: Path): Unit =
    if (Files.exists(path)) {
      if (Files.isDirectory(path))
        scala.util.Using(Files.list(path))(_.toScala(List)).get.foreach(deleteRecursively)
      else path.toFile.setWritable(true).discard()
      Files.delete(path)
    }

  private def withWorkspace(f: (Path, BuildPaths) => Unit): Unit = {
    val workspace = Files.createTempDirectory("bleep-sourcegen-inputs-")
    try f(workspace, BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal))
    finally deleteRecursively(workspace)
  }

  private def writeSchema(workspace: Path, content: String): Path = {
    val schemaDir = workspace.resolve("schema")
    Files.createDirectories(schemaDir)
    Files.writeString(schemaDir.resolve("users.sql"), content)
    schemaDir
  }

  test("sourceGlobs makes an external input part of the consumer's digest") {
    withWorkspace { (workspace, buildPaths) =>
      writeSchema(workspace, "create table users (id int)")

      // `sourceGlobs` is resolved relative to the folder of the project that declares `sourcegen:`,
      // so `../schema` from `myapp` points at <workspace>/schema.
      val myapp = model.Project.empty.copy(sourcegen = model.JsonSet(SortedSet(scriptDef("scripts", "../schema"))))
      val build = makeBuild("scripts" -> model.Project.empty, "myapp" -> myapp)

      val before = ProjectDigest.computeAll(build, buildPaths)(cpn("myapp"))
      writeSchema(workspace, "create table users (id int, email text)")
      val after = ProjectDigest.computeAll(build, buildPaths)(cpn("myapp"))

      after should not be before
    }
  }

  test("without sourceGlobs, the same external directory is invisible to the digest") {
    withWorkspace { (workspace, buildPaths) =>
      writeSchema(workspace, "create table users (id int)")

      val myapp = model.Project.empty.copy(sourcegen = model.JsonSet(SortedSet(scriptDef("scripts"))))
      val build = makeBuild("scripts" -> model.Project.empty, "myapp" -> myapp)

      val before = ProjectDigest.computeAll(build, buildPaths)(cpn("myapp"))
      writeSchema(workspace, "create table users (id int, email text)")
      val after = ProjectDigest.computeAll(build, buildPaths)(cpn("myapp"))

      // Nothing in the build mentions `schema/`, so nothing should. This is the control for the test above.
      after shouldBe before
    }
  }

  test("sourceGlobs on one consumer does not leak into another consumer's digest") {
    withWorkspace { (workspace, buildPaths) =>
      writeSchema(workspace, "create table users (id int)")

      val declares = model.Project.empty.copy(sourcegen = model.JsonSet(SortedSet(scriptDef("scripts", "../schema"))))
      val doesNot = model.Project.empty.copy(sourcegen = model.JsonSet(SortedSet(scriptDef("scripts"))))
      val build = makeBuild("scripts" -> model.Project.empty, "declares" -> declares, "does-not" -> doesNot)

      val before = ProjectDigest.computeAll(build, buildPaths)
      writeSchema(workspace, "create table users (id int, email text)")
      val after = ProjectDigest.computeAll(build, buildPaths)

      after(cpn("declares")) should not be before(cpn("declares"))
      after(cpn("does-not")) shouldBe before(cpn("does-not"))
    }
  }

  test("the same directory declared as `sources` on the script project also enters the consumer's digest") {
    withWorkspace { (workspace, buildPaths) =>
      writeSchema(workspace, "create table users (id int)")

      // `sources` is resolved relative to the script project's folder (<workspace>/scripts).
      val scripts = model.Project.empty.copy(sources = model.JsonSet(SortedSet(RelPath.force("../schema"))))
      val myapp = model.Project.empty.copy(sourcegen = model.JsonSet(SortedSet(scriptDef("scripts"))))
      val build = makeBuild("scripts" -> scripts, "myapp" -> myapp)

      val before = ProjectDigest.computeAll(build, buildPaths)(cpn("myapp"))
      writeSchema(workspace, "create table users (id int, email text)")
      val after = ProjectDigest.computeAll(build, buildPaths)(cpn("myapp"))

      after should not be before
    }
  }

  test("`resources` on the script project works the same way") {
    withWorkspace { (workspace, buildPaths) =>
      writeSchema(workspace, "create table users (id int)")

      val scripts = model.Project.empty.copy(resources = model.JsonSet(SortedSet(RelPath.force("../schema"))))
      val myapp = model.Project.empty.copy(sourcegen = model.JsonSet(SortedSet(scriptDef("scripts"))))
      val build = makeBuild("scripts" -> scripts, "myapp" -> myapp)

      val before = ProjectDigest.computeAll(build, buildPaths)(cpn("myapp"))
      writeSchema(workspace, "create table users (id int, email text)")
      val after = ProjectDigest.computeAll(build, buildPaths)(cpn("myapp"))

      after should not be before
    }
  }

  test("a change under the script project's own sources invalidates the consumer") {
    withWorkspace { (workspace, buildPaths) =>
      val srcDir = workspace.resolve("scripts/src/scala")
      Files.createDirectories(srcDir)
      Files.writeString(srcDir.resolve("Main.scala"), "object Main")

      val scripts = model.Project.empty.copy(sources = model.JsonSet(SortedSet(RelPath.force("src/scala"))))
      val myapp = model.Project.empty.copy(sourcegen = model.JsonSet(SortedSet(scriptDef("scripts"))))
      val build = makeBuild("scripts" -> scripts, "myapp" -> myapp)

      val before = ProjectDigest.computeAll(build, buildPaths)(cpn("myapp"))
      Files.writeString(srcDir.resolve("Main.scala"), "object Main { val x = 1 }")
      val after = ProjectDigest.computeAll(build, buildPaths)(cpn("myapp"))

      after should not be before
    }
  }

  test("declaring sourceGlobs changes the digest even before the directory has content, because the declaration is project config") {
    withWorkspace { (_, buildPaths) =>
      val without = model.Project.empty.copy(sourcegen = model.JsonSet(SortedSet(scriptDef("scripts"))))
      val with_ = model.Project.empty.copy(sourcegen = model.JsonSet(SortedSet(scriptDef("scripts", "../schema"))))

      val d1 = ProjectDigest.computeAll(makeBuild("scripts" -> model.Project.empty, "myapp" -> without), buildPaths)(cpn("myapp"))
      val d2 = ProjectDigest.computeAll(makeBuild("scripts" -> model.Project.empty, "myapp" -> with_), buildPaths)(cpn("myapp"))

      d1 should not be d2
    }
  }

  // ============================================================================
  // ProjectInputs — the single definition the four mechanisms share.
  // `bleep build invalidated` (git-diff based) and `--watch` both ask this and nothing else,
  // so covering it here covers them without needing a git repo or a file watcher.
  // ============================================================================

  test("ProjectInputs.all includes directories declared under sourceGlobs") {
    withWorkspace { (workspace, buildPaths) =>
      val myapp = model.Project.empty.copy(
        sources = model.JsonSet(SortedSet(RelPath.force("src/scala"))),
        sourcegen = model.JsonSet(SortedSet(scriptDef("scripts", "../schema")))
      )
      val projectPaths = buildPaths.project(cpn("myapp"), myapp)

      ProjectInputs.all(myapp, projectPaths) should contain(workspace.resolve("schema"))
      ProjectInputs.all(myapp, projectPaths) should contain(workspace.resolve("myapp").resolve("src").resolve("scala"))
    }
  }

  test("ProjectInputs.all on a project without sourcegen is exactly its sources and resources") {
    withWorkspace { (_, buildPaths) =>
      val myapp = model.Project.empty.copy(
        sources = model.JsonSet(SortedSet(RelPath.force("src/scala"))),
        resources = model.JsonSet(SortedSet(RelPath.force("src/resources")))
      )
      val projectPaths = buildPaths.project(cpn("myapp"), myapp)

      ProjectInputs.all(myapp, projectPaths) shouldBe (projectPaths.sourcesDirs.all ++ projectPaths.resourcesDirs.all)
    }
  }
}
