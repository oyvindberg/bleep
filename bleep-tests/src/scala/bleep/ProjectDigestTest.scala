package bleep

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.collection.immutable.SortedSet
import scala.jdk.StreamConverters.*

class ProjectDigestTest extends AnyFunSuite with Matchers {

  private def cpn(name: String): model.CrossProjectName =
    model.CrossProjectName(model.ProjectName(name), None)

  // None of this suite's fixtures put a `${HOME_DIR}`/`${TEMP_DIR}` token in any option, so the only thing that matters about this is that every call site
  // in this file uses the same one — not that it be built from any particular `UserPaths`.
  private def replacementsFor(buildPaths: BuildPaths): model.Replacements = model.Replacements.paths(buildPaths.buildDir, UserPaths.fromAppDirs)

  private def projectWithDeps(deps: String*): model.Project =
    model.Project.empty.copy(
      dependsOn = model.JsonSet(SortedSet.from(deps.map(d => model.ProjectRef(model.ProjectName(d)))))
    )

  /** The toolchain every test build is pinned to unless it is testing what happens when that changes. */
  private val testJvm: model.Jvm = model.Jvm("temurin:17", None)

  private def makeBuildWith(
      jvm: Option[model.Jvm],
      resolvers: model.JsonList[model.Repository],
      projects: Seq[(String, model.Project)]
  ): model.Build.Exploded =
    model.Build.Exploded(
      $version = model.BleepVersion("test"),
      explodedProjects = projects.map { case (name, p) => cpn(name) -> p }.toMap,
      resolvers = resolvers,
      jvm = jvm,
      scripts = Map.empty,
      remoteCache = None
    )

  private def makeBuild(projects: (String, model.Project)*): model.Build.Exploded =
    makeBuildWith(Some(testJvm), model.JsonList.empty, projects)

  private def createTempWorkspace(): Path = {
    val dir = Files.createTempDirectory("bleep-digest-test-")
    Files.createDirectories(dir.resolve("src"))
    dir
  }

  /** `Files.list` is wrapped in `Using` because the stream holds an open directory handle, and Windows refuses to delete a directory that anyone still has
    * open. The read-only clear is for git: `git init` + `git commit` writes loose objects under `.git/objects` with the read-only attribute set, and on Windows
    * `Files.delete` honours that and throws AccessDeniedException. POSIX only consults the parent directory's permissions, so this went unnoticed.
    */
  private def deleteRecursively(path: Path): Unit =
    if (Files.exists(path)) {
      if (Files.isDirectory(path))
        scala.util.Using(Files.list(path))(_.toScala(List)).get.foreach(deleteRecursively)
      else path.toFile.setWritable(true).discard()
      Files.delete(path)
    }

  test("golden: empty project digest is stable across runs") {
    val workspace = createTempWorkspace()
    try {
      val build = makeBuild("a" -> model.Project.empty)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      val digest = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))

      // This value must remain stable across platforms and versions.
      // If it changes, remote cache entries become invalid.
      // It covers the build-level toolchain (`testJvm`) and the index url it resolves to, as well as the project itself.
      info(s"empty project digest: $digest")
      digest shouldBe "e853707cc6d078301e5ceeae2bab87b0e83801f9a1fcc6c1911470ec7c61980a"
    } finally deleteRecursively(workspace)
  }

  test("golden: project with dep has stable digest") {
    val workspace = createTempWorkspace()
    try {
      val p = model.Project.empty.copy(
        dependencies = model.JsonSet(SortedSet(model.Dep.Java("org.example", "lib", "1.0"): model.Dep))
      )
      val build = makeBuild("a" -> p)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      val digest = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))
      info(s"project-with-dep digest: $digest")
      digest shouldBe "5f65e20cccda0e0d1fd1dd7a34e600edf5aaf4788a8d2d0b44ef01b5bb64f74a"
    } finally deleteRecursively(workspace)
  }

  test("identical projects produce identical digests") {
    val workspace = createTempWorkspace()
    try {
      val build = makeBuild("a" -> model.Project.empty)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      val digests1 = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))
      val digests2 = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))

      digests1(cpn("a")) shouldBe digests2(cpn("a"))
    } finally deleteRecursively(workspace)
  }

  test("config changes produce different digests") {
    val workspace = createTempWorkspace()
    try {
      val p1 = model.Project.empty.copy(
        dependencies = model.JsonSet(SortedSet(model.Dep.Java("org.example", "lib", "1.0"): model.Dep))
      )
      val p2 = model.Project.empty.copy(
        dependencies = model.JsonSet(SortedSet(model.Dep.Java("org.example", "lib", "2.0"): model.Dep))
      )
      val build1 = makeBuild("a" -> p1)
      val build2 = makeBuild("a" -> p2)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      val digest1 = ProjectDigest.computeAll(build1, buildPaths, replacementsFor(buildPaths))(cpn("a"))
      val digest2 = ProjectDigest.computeAll(build2, buildPaths, replacementsFor(buildPaths))(cpn("a"))

      digest1 should not be digest2
    } finally deleteRecursively(workspace)
  }

  test("the templates a project was spelled with do not affect its digest, only the settings they carry") {
    val workspace = createTempWorkspace()
    try {
      // exploded: the template's settings are already in the project; only the `extends` naming them differs
      val settled = model.Project.empty.copy(dependencies = model.JsonSet(SortedSet(model.Dep.Java("org.example", "lib", "1.0"): model.Dep)))
      val viaTemplate = settled.copy(`extends` = model.JsonSet(model.TemplateId("template-common")))
      val viaRenamedTemplate = settled.copy(`extends` = model.JsonSet(model.TemplateId("template-renamed")))
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      val digests = List(settled, viaTemplate, viaRenamedTemplate).map(p =>
        ProjectDigest.computeAll(makeBuild("a" -> p), buildPaths, replacementsFor(buildPaths))(cpn("a"))
      )
      digests.distinct should have size 1
    } finally deleteRecursively(workspace)
  }

  test("publish config does not affect digest") {
    val workspace = createTempWorkspace()
    try {
      val p1 = model.Project.empty
      val p2 = model.Project.empty.copy(
        publish = Some(
          model.PublishConfig(
            enabled = None,
            groupId = Some("com.example"),
            description = None,
            url = None,
            organization = None,
            developers = model.JsonSet.empty,
            licenses = model.JsonSet.empty,
            sonatypeProfileName = None,
            sonatypeCredentialHost = None
          )
        )
      )
      val build1 = makeBuild("a" -> p1)
      val build2 = makeBuild("a" -> p2)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      val digest1 = ProjectDigest.computeAll(build1, buildPaths, replacementsFor(buildPaths))(cpn("a"))
      val digest2 = ProjectDigest.computeAll(build2, buildPaths, replacementsFor(buildPaths))(cpn("a"))

      digest1 shouldBe digest2
    } finally deleteRecursively(workspace)
  }

  test("transitive dep changes propagate to dependents") {
    val workspace = createTempWorkspace()
    try {
      val lib1 = model.Project.empty.copy(
        dependencies = model.JsonSet(SortedSet(model.Dep.Java("org.example", "lib", "1.0"): model.Dep))
      )
      val lib2 = model.Project.empty.copy(
        dependencies = model.JsonSet(SortedSet(model.Dep.Java("org.example", "lib", "2.0"): model.Dep))
      )
      val app = projectWithDeps("lib")

      val build1 = makeBuild("lib" -> lib1, "app" -> app)
      val build2 = makeBuild("lib" -> lib2, "app" -> app)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      val appDigest1 = ProjectDigest.computeAll(build1, buildPaths, replacementsFor(buildPaths))(cpn("app"))
      val appDigest2 = ProjectDigest.computeAll(build2, buildPaths, replacementsFor(buildPaths))(cpn("app"))

      appDigest1 should not be appDigest2
    } finally deleteRecursively(workspace)
  }

  test("source file changes produce different digests") {
    val workspace = createTempWorkspace()
    try {
      // sources RelPath is relative to project dir (<buildDir>/<projectName>)
      val srcDir = workspace.resolve("a/src/scala")
      Files.createDirectories(srcDir)
      Files.writeString(srcDir.resolve("Foo.scala"), "object Foo")

      val p = model.Project.empty.copy(
        sources = model.JsonSet(SortedSet(RelPath.force("src/scala")))
      )
      val build = makeBuild("a" -> p)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      val digest1 = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))

      // Change source file
      Files.writeString(srcDir.resolve("Foo.scala"), "object Foo { val x = 1 }")

      val digest2 = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))

      digest1 should not be digest2
    } finally deleteRecursively(workspace)
  }

  test("filesystem hashing matches git hashing after commit") {
    val workspace = Files.createTempDirectory("bleep-digest-git-test-")
    try {
      // Init a fresh git repo
      scala.sys.process.Process(List("git", "init"), workspace.toFile).!!
      scala.sys.process.Process(List("git", "config", "user.email", "test@test.com"), workspace.toFile).!!
      scala.sys.process.Process(List("git", "config", "user.name", "test"), workspace.toFile).!!

      val srcDir = workspace.resolve("a/src/scala")
      val nestedDir = srcDir.resolve("com/example")
      Files.createDirectories(nestedDir)
      Files.writeString(srcDir.resolve("Foo.scala"), "object Foo { val x = 42 }")
      Files.writeString(srcDir.resolve("Bar.scala"), "object Bar")
      // Nested file ensures both code paths produce relPath strings containing a separator,
      // which would expose any OS-native ('\') vs git ('/') mismatch on Windows.
      Files.writeString(nestedDir.resolve("Baz.scala"), "package com.example; object Baz")

      val p = model.Project.empty.copy(
        sources = model.JsonSet(SortedSet(RelPath.force("src/scala")))
      )
      val build = makeBuild("a" -> p)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      // Digest with uncommitted files (uses filesystem hashing)
      val digestBefore = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))

      // Commit everything
      scala.sys.process.Process(List("git", "add", "."), workspace.toFile).!!
      scala.sys.process.Process(List("git", "commit", "-m", "init"), workspace.toFile).!!

      // Digest with committed files (uses git ls-tree hashing)
      val digestAfter = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))

      info(s"before commit (filesystem): $digestBefore")
      info(s"after commit (git ls-tree): $digestAfter")
      digestBefore shouldBe digestAfter
    } finally deleteRecursively(workspace)
  }

  test("golden: nested source paths produce stable cross-OS digest") {
    // Regression for the Windows path-separator bug: ProjectDigest used to feed OS-native
    // separators into the SHA-256 (`\` on Windows, `/` elsewhere). With nested source
    // files the buggy version produced different digests across OSes; the fixed version
    // produces the value asserted below regardless of platform.
    //
    // If this golden value changes, every existing remote-cache entry becomes invalid.
    val workspace = Files.createTempDirectory("bleep-digest-nested-")
    try {
      val srcDir = workspace.resolve("a/src/scala")
      val nestedDir = srcDir.resolve("com/example")
      Files.createDirectories(nestedDir)
      Files.writeString(srcDir.resolve("Foo.scala"), "object Foo { val x = 42 }")
      Files.writeString(nestedDir.resolve("Baz.scala"), "package com.example\nobject Baz")

      val p = model.Project.empty.copy(
        sources = model.JsonSet(SortedSet(RelPath.force("src/scala")))
      )
      val build = makeBuild("a" -> p)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      val digest = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))
      info(s"nested-paths digest: $digest")
      digest shouldBe "0e468fae76ec8911f251c149e65bf9c02772cd3bb3c48765b2e7e6c9f2e1edf3"
    } finally deleteRecursively(workspace)
  }

  test("build-level jvm changes produce different digests") {
    // `jvm:` lives on the build, not on any project, so nothing in the project config YAML moves when the toolchain is bumped.
    // Before this was hashed, a JDK bump left every digest identical and the cache served classes compiled by the old JDK.
    val workspace = createTempWorkspace()
    try {
      val projects = Seq("a" -> model.Project.empty)
      val build17 = makeBuildWith(Some(model.Jvm("temurin:17", None)), model.JsonList.empty, projects)
      val build21 = makeBuildWith(Some(model.Jvm("temurin:21", None)), model.JsonList.empty, projects)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      val digest17 = ProjectDigest.computeAll(build17, buildPaths, replacementsFor(buildPaths))(cpn("a"))
      val digest21 = ProjectDigest.computeAll(build21, buildPaths, replacementsFor(buildPaths))(cpn("a"))

      digest17 should not be digest21
    } finally deleteRecursively(workspace)
  }

  test("jvm index is part of the digest, and the default index is not a change") {
    val workspace = createTempWorkspace()
    try {
      val projects = Seq("a" -> model.Project.empty)
      val defaultIndex = makeBuildWith(Some(model.Jvm("temurin:17", None)), model.JsonList.empty, projects)
      val explicitDefaultIndex =
        makeBuildWith(Some(model.Jvm("temurin:17", Some(coursier.jvm.JvmChannel.gitHubIndexUrl))), model.JsonList.empty, projects)
      val otherIndex = makeBuildWith(Some(model.Jvm("temurin:17", Some("https://corp.example.com/jvm-index.json"))), model.JsonList.empty, projects)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      val digestDefault = ProjectDigest.computeAll(defaultIndex, buildPaths, replacementsFor(buildPaths))(cpn("a"))
      val digestExplicitDefault = ProjectDigest.computeAll(explicitDefaultIndex, buildPaths, replacementsFor(buildPaths))(cpn("a"))
      val digestOther = ProjectDigest.computeAll(otherIndex, buildPaths, replacementsFor(buildPaths))(cpn("a"))

      // Spelling out the index coursier would have used anyway describes the same toolchain
      digestExplicitDefault shouldBe digestDefault
      // A different index can map the same name to a different JDK
      digestOther should not be digestDefault
    } finally deleteRecursively(workspace)
  }

  test("a build without a jvm cannot be digested") {
    val workspace = createTempWorkspace()
    try {
      val build = makeBuildWith(None, model.JsonList.empty, Seq("a" -> model.Project.empty))
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      val th = the[BleepException] thrownBy ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))
      th.getMessage should include("jvm:")
    } finally deleteRecursively(workspace)
  }

  test("resolvers do not affect digest") {
    // Deliberate: a coordinate resolves to the same bytes from any repository, and a missing artifact fails resolution
    // hard rather than compiling something else. Hashing resolvers would invalidate the whole build whenever someone
    // adds a repository for one dependency. See the ProjectDigest scaladoc before changing this.
    val workspace = createTempWorkspace()
    try {
      val projects = Seq("a" -> model.Project.empty)
      val noResolvers = makeBuildWith(Some(testJvm), model.JsonList.empty, projects)
      val withResolvers = makeBuildWith(
        Some(testJvm),
        model.JsonList(List(model.Repository.Maven(None, new java.net.URI("https://corp.example.com/maven")): model.Repository)),
        projects
      )
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      val digestWithout = ProjectDigest.computeAll(noResolvers, buildPaths, replacementsFor(buildPaths))(cpn("a"))
      val digestWith = ProjectDigest.computeAll(withResolvers, buildPaths, replacementsFor(buildPaths))(cpn("a"))

      digestWith shouldBe digestWithout
    } finally deleteRecursively(workspace)
  }

  test("resource changes affect digest") {
    val workspace = createTempWorkspace()
    try {
      val resDir = workspace.resolve("a/src/resources")
      Files.createDirectories(resDir)
      Files.writeString(resDir.resolve("app.conf"), "key=value1")

      val p = model.Project.empty.copy(
        resources = model.JsonSet(SortedSet(RelPath.force("src/resources")))
      )
      val build = makeBuild("a" -> p)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      val digest1 = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))

      Files.writeString(resDir.resolve("app.conf"), "key=value2")

      val digest2 = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))

      digest1 should not be digest2
    } finally deleteRecursively(workspace)
  }

  /** A git repo with `user.*` set, so committing works regardless of the machine's global config. */
  private def gitInit(workspace: Path): Unit = {
    scala.sys.process.Process(List("git", "init"), workspace.toFile).!!.discard()
    scala.sys.process.Process(List("git", "config", "user.email", "test@test.com"), workspace.toFile).!!.discard()
    scala.sys.process.Process(List("git", "config", "user.name", "test"), workspace.toFile).!!.discard()
  }

  private def gitCommitAll(workspace: Path): Unit = {
    scala.sys.process.Process(List("git", "add", "."), workspace.toFile).!!.discard()
    scala.sys.process.Process(List("git", "commit", "-m", "init"), workspace.toFile).!!.discard()
  }

  // Names git considers unusual enough to quote in `--porcelain` / `ls-tree` output unless `-z` is passed.
  // A space alone is enough; the non-ASCII one additionally gets octal-escaped ("caf\303\251.txt").
  private val quotedNames = List("with space.scala", "caf\u00e9.scala")

  test("uncommitted changes to files git quotes still move the digest") {
    // Regression: `git status --porcelain` without `-z` returns these paths quoted and octal-escaped.
    // The parser resolved that spelling against buildDir, got a path matching nothing on disk, and so found
    // no dirty file under the source directory — which sent hashDirectories down the `git ls-tree` fast path,
    // hashing HEAD's blobs instead of the modified working tree. The digest stayed put while the file moved,
    // and `remote-cache push` stored classes under a key describing content other than what was compiled.
    quotedNames.foreach { name =>
      val workspace = Files.createTempDirectory("bleep-digest-quoted-")
      try {
        gitInit(workspace)
        val srcDir = workspace.resolve("a/src/scala")
        Files.createDirectories(srcDir)
        Files.writeString(srcDir.resolve(name), "object Foo")
        gitCommitAll(workspace)

        val p = model.Project.empty.copy(sources = model.JsonSet(SortedSet(RelPath.force("src/scala"))))
        val build = makeBuild("a" -> p)
        val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

        val committed = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))

        Files.writeString(srcDir.resolve(name), "object Foo { val x = 1 }")
        val modified = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))

        withClue(s"digest did not move when '$name' was modified: ") {
          modified should not be committed
        }
      } finally deleteRecursively(workspace)
    }
  }

  test("a gitignored file inside a dirty directory is not an input") {
    // The filesystem fallback (hashFilesystem) used to be a raw Files.walk with no idea what git would consider part of the project. Content a project
    // explicitly excluded via .gitignore — build junk, IDE state, or (the real-world case this was found from) a nested git worktree sitting inside a
    // declared directory — got hashed right along with everything else, just because the directory was "dirty" (some OTHER, non-ignored file in it
    // changed) and took the filesystem path instead of git ls-tree's.
    val workspace = Files.createTempDirectory("bleep-digest-gitignore-")
    try {
      gitInit(workspace)
      val srcDir = workspace.resolve("a/src/scala")
      Files.createDirectories(srcDir)
      Files.writeString(srcDir.resolve("Foo.scala"), "object Foo")
      Files.writeString(workspace.resolve(".gitignore"), "ignored.scala\n")
      gitCommitAll(workspace)

      val p = model.Project.empty.copy(sources = model.JsonSet(SortedSet(RelPath.force("src/scala"))))
      val build = makeBuild("a" -> p)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      // Make the directory dirty via an ordinary, non-ignored change, and plant ignored content alongside it.
      Files.writeString(srcDir.resolve("Foo.scala"), "object Foo { val x = 1 }")
      Files.writeString(srcDir.resolve("ignored.scala"), "object ShouldNeverBeHashed")
      val before = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))

      // Changing ONLY the ignored file's content must leave the digest alone.
      Files.writeString(srcDir.resolve("ignored.scala"), "object StillShouldNeverBeHashed")
      val after = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))

      after shouldBe before
    } finally deleteRecursively(workspace)
  }

  test("a nested git worktree sitting inside a declared directory is never read, gitignored or not") {
    // The real-world case: a monorepo with dozens of other worktrees checked out inside its own directory tree (always gitignored — nobody commits a
    // worktree checkout), where a project's declared directory happens to be broad enough to be an ancestor of one. Modeled here with a plain nested
    // directory carrying its own `.git` file, exactly the shape a worktree (or any embedded repository) has on disk — this doesn't need a real `git
    // worktree add` to prove the point, since the guard only looks for that one marker.
    val workspace = Files.createTempDirectory("bleep-digest-nested-worktree-")
    try {
      gitInit(workspace)
      val srcDir = workspace.resolve("a/src/scala")
      Files.createDirectories(srcDir)
      Files.writeString(srcDir.resolve("Foo.scala"), "object Foo")
      // `sources:` resolves relative to project `a`'s own folder (`workspace/a/`), same convention the other tests here rely on — the pattern needs that
      // prefix too, since it contains a `/` and is therefore anchored to the repo root rather than matched at every directory level.
      Files.writeString(workspace.resolve(".gitignore"), "a/src/scala/nested-worktree/\n")
      gitCommitAll(workspace)

      val p = model.Project.empty.copy(sources = model.JsonSet(SortedSet(RelPath.force("src/scala"))))
      val build = makeBuild("a" -> p)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      val nestedWorktree = srcDir.resolve("nested-worktree")
      Files.createDirectories(nestedWorktree)
      Files.writeString(nestedWorktree.resolve(".git"), "gitdir: /elsewhere/.git/worktrees/whatever\n")
      Files.writeString(nestedWorktree.resolve("SomeFileInTheOtherWorktree.scala"), "object WhateverElseIsCheckedOutThere")

      // Dirty `srcDir` via an ordinary change, same as the test above, so the filesystem fallback actually runs.
      Files.writeString(srcDir.resolve("Foo.scala"), "object Foo { val x = 1 }")
      val before = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))

      Files.writeString(nestedWorktree.resolve("SomeFileInTheOtherWorktree.scala"), "object SomethingCompletelyDifferentNow")
      val after = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))

      after shouldBe before
    } finally deleteRecursively(workspace)
  }

  test("with no git repository at all, the filesystem fallback still refuses to descend into a nested repository's own working directory") {
    // gitLsFilesUnder answers `None` here (no git repository to ask at all), so there are no ignore rules to lean on — the one case walkIgnoringNestedRepos
    // exists for: an explicit check for a nested `.git`, independent of any ignore configuration.
    val workspace = createTempWorkspace() // deliberately no gitInit
    try {
      val srcDir = workspace.resolve("a/src/scala")
      Files.createDirectories(srcDir)
      Files.writeString(srcDir.resolve("Foo.scala"), "object Foo")

      val nestedWorktree = srcDir.resolve("nested-worktree")
      Files.createDirectories(nestedWorktree)
      Files.writeString(nestedWorktree.resolve(".git"), "gitdir: /elsewhere/.git/worktrees/whatever\n")
      Files.writeString(nestedWorktree.resolve("SomeFileInTheOtherWorktree.scala"), "object WhateverElseIsCheckedOutThere")

      val p = model.Project.empty.copy(sources = model.JsonSet(SortedSet(RelPath.force("src/scala"))))
      val build = makeBuild("a" -> p)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      val before = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))
      Files.writeString(nestedWorktree.resolve("SomeFileInTheOtherWorktree.scala"), "object SomethingCompletelyDifferentNow")
      val after = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))

      after shouldBe before
    } finally deleteRecursively(workspace)
  }

  test("digest of a committed tree does not depend on core.quotePath") {
    // Regression: `git ls-tree` obeys `core.quotePath`, so the *spelling* of a non-ASCII path went into the
    // digest as either `café.scala` or `"caf\303\251.txt"` depending on a setting in the developer's own
    // ~/.gitconfig. Same commit, same content, two cache keys. `-z` never quotes, so the setting cannot reach us.
    val workspace = Files.createTempDirectory("bleep-digest-quotepath-")
    try {
      gitInit(workspace)
      val srcDir = workspace.resolve("a/src/scala")
      Files.createDirectories(srcDir)
      Files.writeString(srcDir.resolve("caf\u00e9.scala"), "object Cafe")
      gitCommitAll(workspace)

      val p = model.Project.empty.copy(sources = model.JsonSet(SortedSet(RelPath.force("src/scala"))))
      val build = makeBuild("a" -> p)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      def withQuotePath(value: String): String = {
        scala.sys.process.Process(List("git", "config", "core.quotePath", value), workspace.toFile).!!.discard()
        ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))
      }

      withQuotePath("false") shouldBe withQuotePath("true")
    } finally deleteRecursively(workspace)
  }

  test("a rename marks both ends dirty") {
    // `-z` reports a rename as two NUL-terminated fields rather than "old -> new" on one line. Miscounting
    // those fields desynchronises the whole parse, so assert the source path — which the old " -> " split
    // dropped entirely — is seen as dirty too.
    val workspace = Files.createTempDirectory("bleep-digest-rename-")
    try {
      gitInit(workspace)
      val fromDir = workspace.resolve("a/src/scala")
      val toDir = workspace.resolve("b/src/scala")
      Files.createDirectories(fromDir)
      Files.createDirectories(toDir)
      Files.writeString(fromDir.resolve("Foo.scala"), "object Foo")
      Files.writeString(toDir.resolve("Keep.scala"), "object Keep")
      gitCommitAll(workspace)

      val p = model.Project.empty.copy(sources = model.JsonSet(SortedSet(RelPath.force("src/scala"))))
      val build = makeBuild("a" -> p, "b" -> p)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      val before = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))

      // Staged, so git reports it as a rename rather than as a deletion plus an untracked file.
      scala.sys.process.Process(List("git", "mv", "a/src/scala/Foo.scala", "b/src/scala/Foo.scala"), workspace.toFile).!!.discard()

      val after = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))

      // `a` lost a file: only correct if the rename's *source* field was parsed.
      withClue("source of the rename: ")(after(cpn("a")) should not be before(cpn("a")))
      withClue("destination of the rename: ")(after(cpn("b")) should not be before(cpn("b")))
    } finally deleteRecursively(workspace)
  }

  /** Write `content` to `dir/name`, creating whatever directories that needs. */
  private def writeIn(dir: Path, name: String, content: String): Unit = {
    val file = dir.resolve(name)
    Files.createDirectories(file.getParent)
    Files.writeString(file, content).discard()
  }

  private val javaWithAnnotationProcessing: model.Java =
    model.Java(
      options = model.Options.empty,
      scanForAnnotationProcessors = Some(model.ScanForAnnotationProcessors.Yes),
      annotationProcessors = model.JsonSet.empty,
      annotationProcessorOptions = model.AnnotationProcessorOptions.empty,
      ecjVersion = None
    )

  private val kotlinWithSymbolProcessing: model.Kotlin =
    model.Kotlin(
      version = None,
      options = model.Options.empty,
      jvmTarget = None,
      compilerPlugins = model.JsonSet.empty,
      kspVersion = None,
      scanForSymbolProcessors = Some(true),
      symbolProcessors = model.JsonSet.empty,
      symbolProcessorOptions = model.SymbolProcessorOptions.empty,
      js = None,
      native = None
    )

  test("annotation-processor output is not an input: rewriting it leaves the digest alone") {
    // It is a function of the sources and the processor coordinates, both already hashed — a macro expansion that happens to be written to a file. Keeping it
    // in the key bought one thing: any processor embedding a timestamp (MapStruct does by default, unless `suppressGeneratorTimestamp` is set) moved the
    // digest on every build and left the project and everything downstream permanently cold.
    val workspace = createTempWorkspace()
    try {
      val p = model.Project.empty.copy(java = Some(javaWithAnnotationProcessing))
      val build = makeBuild("a" -> p)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)
      val apDir = buildPaths.generatedSourcesDir(cpn("a"), "annotations")
      val projectPaths = buildPaths.project(cpn("a"), p, build.crossPlatforms(cpn("a").name), replacementsFor(buildPaths))
      assert(projectPaths.sourcesDirs.all(Usage.Compile).contains(apDir), "the compiler must still see AP output")
      // The same answer reaches `--watch` and `build invalidated` through `ProjectInputs`, not just the digest.
      assert(!ProjectInputs.all(p, projectPaths).contains(apDir), "AP output is not an input to watch or invalidate on")

      writeIn(apDir, "FooMapperImpl.java", "@Generated(date = \"2026-09-21T10:00:00\") class FooMapperImpl {}")
      val before = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))

      writeIn(apDir, "FooMapperImpl.java", "@Generated(date = \"2026-09-21T10:00:01\") class FooMapperImpl {}")
      val after = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))

      after shouldBe before
    } finally deleteRecursively(workspace)
  }

  test("KSP output is not an input, on the sources side or the resources side") {
    // KSP writes to both `sourcesDirs.ksp` (kotlin/, java/) and `resourcesDirs.ksp` (resources/) — separate fields, separate hashing steps — so both are
    // exercised rather than trusting that one covers the other.
    val workspace = createTempWorkspace()
    try {
      val p = model.Project.empty.copy(kotlin = Some(kotlinWithSymbolProcessing))
      val build = makeBuild("a" -> p)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)
      val kspBase = buildPaths.generatedSourcesDir(cpn("a"), "ksp")

      writeIn(kspBase / "kotlin", "Generated.kt", "object Generated { val n = 1 }")
      writeIn(kspBase / "resources", "META-INF/generated.txt", "one")
      val before = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))

      writeIn(kspBase / "kotlin", "Generated.kt", "object Generated { val n = 2 }")
      writeIn(kspBase / "resources", "META-INF/generated.txt", "two")
      val after = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))

      after shouldBe before
    } finally deleteRecursively(workspace)
  }

  test("sourcegen output is not hashed directly: only its declared inputs are, via its own generator digest") {
    // Unlike AP/KSP output (not an input at all, above), a sourcegen's output IS an input to the digest — but folded via its own generator digest
    // (`ProjectDigest.computeGeneratorDigest`: the script project's digest, each declared `inputs` project's digest, and `sourceGlobs` content), never by
    // hashing the output bytes directly. That equivalence is deliberate, not an approximation: a generator is required to be a deterministic function of
    // exactly those inputs for any of this digest's existing invalidation to be sound at all (today's digest already trusts the Scala/Java compiler the
    // same way). It's also what lets a project's digest be computed without the generator ever having run — see `bleep remote-cache`. So rewriting the
    // output directly, with nothing about its declared inputs changing, must leave the digest alone — the opposite of what hashing the raw bytes would
    // have done, and exactly why `sourceGlobs`/the script project's own sources moving the digest (covered in SourcegenInputsTest) is still true.
    val workspace = createTempWorkspace()
    try {
      val gen: model.ScriptDef = model.ScriptDef.Main(cpn("scripts"), "my.Gen", model.JsonSet.empty, model.JsonSet.empty, None)
      val p = model.Project.empty.copy(sourcegen = model.JsonSet(SortedSet(gen)))
      val build = makeBuild("scripts" -> model.Project.empty, "a" -> p)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)
      val genDir = buildPaths.generatedSourcesDir(cpn("a"), gen.folderName)

      writeIn(genDir, "Version.java", "class Version { static String V = \"1.0.0\"; }")
      val before = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))

      writeIn(genDir, "Version.java", "class Version { static String V = \"1.0.1\"; }")
      val after = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))

      after shouldBe before
    } finally deleteRecursively(workspace)
  }

  test("a stamp file is not an input: rewriting it leaves the digest alone") {
    // The premise of `stamp:`. `project-digest` is itself a stamp, so if the file were hashed the digest would be an input to itself; for `dynver`, a value that
    // changes on every commit would make this project and everything downstream a permanent cache miss.
    val workspace = createTempWorkspace()
    try {
      val p = model.Project.empty.copy(stamp = model.JsonSet(model.StampKind.Dynver))
      val build = makeBuild("a" -> p)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)
      val stampsRoot = buildPaths
        .project(cpn("a"), p, build.crossPlatforms(cpn("a").name), replacementsFor(buildPaths))
        .resourcesDirs
        .stamps
        .getOrElse(fail("a declared stamps but has no stamps directory"))

      writeIn(stampsRoot, StampFile.resourcePath(cpn("a")), "dynver=1.0.0+1-aaaaaaa\n")
      val before = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))

      writeIn(stampsRoot, StampFile.resourcePath(cpn("a")), "dynver=1.0.0+2-bbbbbbb\n")
      val after = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a"))

      after shouldBe before
    } finally deleteRecursively(workspace)
  }

  /** `git rev-parse HEAD`, trimmed. */
  private def currentCommit(workspace: Path): String =
    scala.sys.process.Process(List("git", "rev-parse", "HEAD"), workspace.toFile).!!.trim

  test("computeAllAt a ref that is HEAD, with a clean tree, agrees with computeAll") {
    val workspace = Files.createTempDirectory("bleep-digest-at-head-")
    try {
      gitInit(workspace)
      val srcDir = workspace.resolve("a/src/scala")
      Files.createDirectories(srcDir)
      Files.writeString(srcDir.resolve("Foo.scala"), "object Foo")
      gitCommitAll(workspace)

      val p = model.Project.empty.copy(sources = model.JsonSet(SortedSet(RelPath.force("src/scala"))))
      val build = makeBuild("a" -> p)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      val atHead = ProjectDigest.computeAllAt(build, buildPaths, currentCommit(workspace), replacementsFor(buildPaths))
      val current = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))

      atHead(cpn("a")) shouldBe current(cpn("a"))
    } finally deleteRecursively(workspace)
  }

  test("computeAllAt an ancestor commit reproduces the digest that commit actually had, even after a transitive dep moved on") {
    // The whole point of --unsafe-seed-ref: `app` never touches its own source, but depends on `lib`. Once `lib`'s source
    // changes, `app`'s CURRENT digest moves too (dependency propagation), so an exact-digest pull misses for both. The
    // digest computed as of the ancestor commit must match what that commit's digest genuinely was, for both projects,
    // so a cache entry pushed back then is found under the right key.
    val workspace = Files.createTempDirectory("bleep-digest-ancestor-")
    try {
      gitInit(workspace)
      val libSrc = workspace.resolve("lib/src/scala")
      Files.createDirectories(libSrc)
      Files.writeString(libSrc.resolve("Lib.scala"), "object Lib { val v = 1 }")
      gitCommitAll(workspace)
      val commit1 = currentCommit(workspace)

      val lib = model.Project.empty.copy(sources = model.JsonSet(SortedSet(RelPath.force("src/scala"))))
      val app = projectWithDeps("lib")
      val build = makeBuild("lib" -> lib, "app" -> app)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      val digestsAtCommit1 = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))

      // Move lib forward. `app`'s own files never change.
      Files.writeString(libSrc.resolve("Lib.scala"), "object Lib { val v = 2 }")
      gitCommitAll(workspace)

      val digestsNow = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))
      // Sanity: both moved (the existing "transitive dep changes propagate" test covers this for `app`; repeated here
      // because the rest of this test depends on it actually being true).
      digestsNow(cpn("lib")) should not be digestsAtCommit1(cpn("lib"))
      digestsNow(cpn("app")) should not be digestsAtCommit1(cpn("app"))

      val digestsAsOfCommit1 = ProjectDigest.computeAllAt(build, buildPaths, commit1, replacementsFor(buildPaths))
      digestsAsOfCommit1(cpn("lib")) shouldBe digestsAtCommit1(cpn("lib"))
      digestsAsOfCommit1(cpn("app")) shouldBe digestsAtCommit1(cpn("app"))
    } finally deleteRecursively(workspace)
  }

  test("computeAllAt omits a project whose source dir holds untracked content, rather than guessing at its historical bytes") {
    // sourcegen output is exactly this: real content on disk right now that git never saw at any past commit. Silently
    // hashing it under a historical ref's label would produce a digest matching no build that ever ran.
    val workspace = Files.createTempDirectory("bleep-digest-untracked-")
    try {
      gitInit(workspace)
      Files.writeString(workspace.resolve("README.md"), "placeholder")
      gitCommitAll(workspace)
      val commit1 = currentCommit(workspace)

      val genDir = workspace.resolve("a/generated")
      Files.createDirectories(genDir)
      Files.writeString(genDir.resolve("Gen.scala"), "object Gen") // never committed

      val p = model.Project.empty.copy(sources = model.JsonSet(SortedSet(RelPath.force("generated"))))
      val build = makeBuild("a" -> p)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      val atCommit1 = ProjectDigest.computeAllAt(build, buildPaths, commit1, replacementsFor(buildPaths))
      atCommit1.get(cpn("a")) shouldBe None

      // The ordinary, current-tree computation still sees it via the filesystem fallback.
      ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths)).get(cpn("a")) should not be None
    } finally deleteRecursively(workspace)
  }

  test("computeAllAt omits a project whose source dir did not exist yet at that commit") {
    val workspace = Files.createTempDirectory("bleep-digest-new-project-")
    try {
      gitInit(workspace)
      val aSrc = workspace.resolve("a/src/scala")
      Files.createDirectories(aSrc)
      Files.writeString(aSrc.resolve("A.scala"), "object A")
      gitCommitAll(workspace)
      val commit1 = currentCommit(workspace)

      // `b` is created after commit1 — its source dir is untracked at that ref, but has real content on disk now.
      val bSrc = workspace.resolve("b/src/scala")
      Files.createDirectories(bSrc)
      Files.writeString(bSrc.resolve("B.scala"), "object B")

      val projA = model.Project.empty.copy(sources = model.JsonSet(SortedSet(RelPath.force("src/scala"))))
      val projB = model.Project.empty.copy(sources = model.JsonSet(SortedSet(RelPath.force("src/scala"))))
      val build = makeBuild("a" -> projA, "b" -> projB)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      val atCommit1 = ProjectDigest.computeAllAt(build, buildPaths, commit1, replacementsFor(buildPaths))
      atCommit1.get(cpn("a")) should not be None
      atCommit1.get(cpn("b")) shouldBe None
    } finally deleteRecursively(workspace)
  }

  test("computeAllAt resolves a sourcegen-bearing project even though its generated output is untracked — the bug --unsafe-seed-ref was named to fix") {
    // Before generated output was folded via its own generator digest instead of being hashed directly (see computeGeneratorDigest), this came back `None`
    // for exactly the same reason as the untracked-content test above — generated-sources is never git-tracked. That refusal cascaded to every project
    // downstream of a sourcegen dependency, which is what made --unsafe-seed-ref useless on any build that had one at all. It no longer needs to, because
    // resolving the generator's digest never requires reading the generated directory's content, only the generator's own declared inputs (here: none
    // beyond the script project itself, which is tracked like any other project).
    val workspace = Files.createTempDirectory("bleep-digest-sourcegen-at-ref-")
    try {
      gitInit(workspace)
      val scriptsSrc = workspace.resolve("scripts/src/scala")
      Files.createDirectories(scriptsSrc)
      Files.writeString(scriptsSrc.resolve("Gen.scala"), "object Gen")
      val libSrc = workspace.resolve("lib/src/scala")
      Files.createDirectories(libSrc)
      Files.writeString(libSrc.resolve("Lib.scala"), "object Lib")
      Files.writeString(workspace.resolve("unrelated.txt"), "v1") // a tracked file `a` doesn't declare, so changing it moves nothing of a's
      gitCommitAll(workspace)
      val commit1 = currentCommit(workspace)

      val gen: model.ScriptDef = model.ScriptDef.Main(cpn("scripts"), "my.Gen", model.JsonSet.empty, model.JsonSet.empty, None)
      val scripts = model.Project.empty.copy(sources = model.JsonSet(SortedSet(RelPath.force("src/scala"))))
      val lib = model.Project.empty.copy(sources = model.JsonSet(SortedSet(RelPath.force("src/scala"))))
      // `dependsOn: lib` alongside `sourcegen:` mirrors the real shape that caught the fold-order bug: a project whose digest folds BOTH an ordinary
      // transitive dependency AND a sourcegen declaration. A project with only one or the other can't tell two different fold orders apart.
      val a = projectWithDeps("lib").copy(sourcegen = model.JsonSet(SortedSet(gen)))
      val build = makeBuild("scripts" -> scripts, "lib" -> lib, "a" -> a)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      // Generated output exists on disk (as it would after a real local run), but was never committed — this is the untracked content that used to cause
      // the refusal.
      val genDir = buildPaths.generatedSourcesDir(cpn("a"), gen.folderName)
      writeIn(genDir, "Generated.scala", "object Generated")

      val atCommit1 = ProjectDigest.computeAllAt(build, buildPaths, commit1, replacementsFor(buildPaths))
      atCommit1.get(cpn("a")) should not be None

      // The working tree is still exactly commit1, so this isn't just "resolvable" — it must equal what computeAll produces right now, directly pinning
      // computeAllAt's whole documented job (reproduce a digest a real push could have produced). This caught a real bug: computeAll folds a project's
      // sourcegen-declaration digests BEFORE its transitive/indirect deps, but computeAllAt folded them AFTER — same information, different
      // MessageDigest.update order, so the two disagreed on every sourcegen-bearing project despite both being "correct" in isolation.
      ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a")) shouldBe atCommit1(cpn("a"))

      // Move the repo on — the unrelated file changes, `a`'s and `scripts`' own content doesn't — then check that `atCommit1`'s digest for `a` still
      // matches a direct computeAll at commit1 (the sibling ancestor-commit test above already pins that property for ordinary projects; this just
      // confirms the refusal is gone for a sourcegen-bearing one too).
      Files.writeString(workspace.resolve("unrelated.txt"), "v2")
      gitCommitAll(workspace)
      val atCommit1Again = ProjectDigest.computeAllAt(build, buildPaths, commit1, replacementsFor(buildPaths))
      atCommit1Again(cpn("a")) shouldBe atCommit1(cpn("a"))
    } finally deleteRecursively(workspace)
  }

  /** `git submodule add` with a plain local path. Since CVE-2022-39253, git refuses `file://` submodule sources by default; a bare local path isn't that
    * transport, but `-c protocol.file.allow=always` is harmless insurance against a stricter global config on the machine running the test.
    */
  private def gitSubmoduleAdd(workspace: Path, sourceRepo: Path, at: String): Unit =
    scala.sys.process
      .Process(
        List("git", "-c", "protocol.file.allow=always", "submodule", "add", sourceRepo.toString, at),
        workspace.toFile
      )
      .!!
      .discard()

  test("computeAllAt resolves a project sourcing from inside a git submodule, at the commit the superproject actually pinned") {
    // `git ls-tree` never descends into a submodule's content from the superproject — not even with `-r` — so a project's source directory several levels
    // inside one (typical: nobody sources directly from a submodule's own root) looked exactly like untracked content to the refusal above, for reasons
    // that have nothing to do with sourcegen. Real-world case: bleep's own `liberated/zinc`, `liberated/librarymanagement`, etc.
    val submoduleSrc = Files.createTempDirectory("bleep-digest-submodule-src-")
    val workspace = Files.createTempDirectory("bleep-digest-submodule-ws-")
    try {
      // The "upstream" repo the submodule points at.
      gitInit(submoduleSrc)
      val libSrc = submoduleSrc.resolve("src/scala")
      Files.createDirectories(libSrc)
      Files.writeString(libSrc.resolve("Lib.scala"), "object Lib { val v = 1 }")
      gitCommitAll(submoduleSrc)

      // The superproject. Project `a`'s own folder defaults to `workspace/a/` (same convention every other test here relies on), and `sources:` is
      // relative to it, so the submodule is mounted at `a/liberated/lib` — several levels above where `a` actually declares its sources underneath it.
      gitInit(workspace)
      gitSubmoduleAdd(workspace, submoduleSrc, "a/liberated/lib")
      gitCommitAll(workspace) // commits the gitlink + .gitmodules
      val commit1 = currentCommit(workspace)

      val p = model.Project.empty.copy(sources = model.JsonSet(SortedSet(RelPath.force("liberated/lib/src/scala"))))
      val build = makeBuild("a" -> p)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      val atCommit1 = ProjectDigest.computeAllAt(build, buildPaths, commit1, replacementsFor(buildPaths))
      atCommit1.get(cpn("a")) should not be None
      // Same correctness bar as the sourcegen test above: not just "resolvable", but equal to what computeAll produces right now, since the working tree
      // is still exactly commit1.
      ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))(cpn("a")) shouldBe atCommit1(cpn("a"))

      // Move the submodule forward (a real commit inside it, then re-point the superproject's gitlink at it) and re-pin it in the superproject.
      // `git submodule add` leaves the submodule on a detached HEAD, so `fetch` + `checkout <sha>` rather than `pull` (which needs a tracking branch).
      Files.writeString(libSrc.resolve("Lib.scala"), "object Lib { val v = 2 }")
      gitCommitAll(submoduleSrc)
      val submoduleCommit2 = currentCommit(submoduleSrc)
      val submoduleWorkdir = workspace.resolve("a/liberated/lib")
      scala.sys.process.Process(List("git", "fetch", "origin"), submoduleWorkdir.toFile).!!.discard()
      scala.sys.process.Process(List("git", "checkout", submoduleCommit2), submoduleWorkdir.toFile).!!.discard()
      gitCommitAll(workspace)
      val commit2 = currentCommit(workspace)

      val digestsNow = ProjectDigest.computeAll(build, buildPaths, replacementsFor(buildPaths))
      digestsNow(cpn("a")) should not be atCommit1(cpn("a"))

      // The real test of historical fidelity, not just "doesn't refuse": computeAllAt at commit1, asked for again from a workspace that has since moved
      // on, must reproduce commit1's own bytes — the submodule's content as it was BEFORE the bump — not whatever the submodule happens to be checked out
      // to right now (commit2's content, which is what's actually sitting on disk at this point in the test).
      val atCommit1Again = ProjectDigest.computeAllAt(build, buildPaths, commit1, replacementsFor(buildPaths))
      atCommit1Again(cpn("a")) shouldBe atCommit1(cpn("a"))
      atCommit1Again(cpn("a")) should not be digestsNow(cpn("a"))

      // And computeAllAt at the NEW commit must agree with the NEW computeAll, the same property one more time at the other end.
      val atCommit2 = ProjectDigest.computeAllAt(build, buildPaths, commit2, replacementsFor(buildPaths))
      atCommit2(cpn("a")) shouldBe digestsNow(cpn("a"))
    } finally {
      deleteRecursively(workspace)
      deleteRecursively(submoduleSrc)
    }
  }

  test("declaring stamps leaves the digest alone") {
    // `stamp:` only decides what gets written into the artifact, so like `publish` it is stripped from the config that is hashed. Turning a stamp on must not
    // evict the project's cache entry.
    val workspace = createTempWorkspace()
    try {
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)
      val plain = ProjectDigest.computeAll(makeBuild("a" -> model.Project.empty), buildPaths, replacementsFor(buildPaths))(cpn("a"))
      val stamped = ProjectDigest.computeAll(
        makeBuild("a" -> model.Project.empty.copy(stamp = model.JsonSet(model.StampKind.Dynver, model.StampKind.ProjectDigest))),
        buildPaths,
        replacementsFor(buildPaths)
      )(cpn("a"))

      stamped shouldBe plain
    } finally deleteRecursively(workspace)
  }
}
