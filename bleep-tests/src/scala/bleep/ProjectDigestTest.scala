package bleep

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.collection.immutable.SortedSet
import scala.jdk.StreamConverters.*

class ProjectDigestTest extends AnyFunSuite with Matchers {

  private def cpn(name: String): model.CrossProjectName =
    model.CrossProjectName(model.ProjectName(name), None)

  private def projectWithDeps(deps: String*): model.Project =
    model.Project.empty.copy(
      dependsOn = model.JsonSet(SortedSet.from(deps.map(model.ProjectName.apply)))
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

      val digest = ProjectDigest.computeAll(build, buildPaths)(cpn("a"))

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

      val digest = ProjectDigest.computeAll(build, buildPaths)(cpn("a"))
      info(s"project-with-dep digest: $digest")
      digest shouldBe "5f65e20cccda0e0d1fd1dd7a34e600edf5aaf4788a8d2d0b44ef01b5bb64f74a"
    } finally deleteRecursively(workspace)
  }

  test("identical projects produce identical digests") {
    val workspace = createTempWorkspace()
    try {
      val build = makeBuild("a" -> model.Project.empty)
      val buildPaths = BuildPaths(workspace, BuildLoader.inDirectory(workspace), model.BuildVariant.Normal)

      val digests1 = ProjectDigest.computeAll(build, buildPaths)
      val digests2 = ProjectDigest.computeAll(build, buildPaths)

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

      val digest1 = ProjectDigest.computeAll(build1, buildPaths)(cpn("a"))
      val digest2 = ProjectDigest.computeAll(build2, buildPaths)(cpn("a"))

      digest1 should not be digest2
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

      val digest1 = ProjectDigest.computeAll(build1, buildPaths)(cpn("a"))
      val digest2 = ProjectDigest.computeAll(build2, buildPaths)(cpn("a"))

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

      val appDigest1 = ProjectDigest.computeAll(build1, buildPaths)(cpn("app"))
      val appDigest2 = ProjectDigest.computeAll(build2, buildPaths)(cpn("app"))

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

      val digest1 = ProjectDigest.computeAll(build, buildPaths)(cpn("a"))

      // Change source file
      Files.writeString(srcDir.resolve("Foo.scala"), "object Foo { val x = 1 }")

      val digest2 = ProjectDigest.computeAll(build, buildPaths)(cpn("a"))

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
      val digestBefore = ProjectDigest.computeAll(build, buildPaths)(cpn("a"))

      // Commit everything
      scala.sys.process.Process(List("git", "add", "."), workspace.toFile).!!
      scala.sys.process.Process(List("git", "commit", "-m", "init"), workspace.toFile).!!

      // Digest with committed files (uses git ls-tree hashing)
      val digestAfter = ProjectDigest.computeAll(build, buildPaths)(cpn("a"))

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

      val digest = ProjectDigest.computeAll(build, buildPaths)(cpn("a"))
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

      val digest17 = ProjectDigest.computeAll(build17, buildPaths)(cpn("a"))
      val digest21 = ProjectDigest.computeAll(build21, buildPaths)(cpn("a"))

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

      val digestDefault = ProjectDigest.computeAll(defaultIndex, buildPaths)(cpn("a"))
      val digestExplicitDefault = ProjectDigest.computeAll(explicitDefaultIndex, buildPaths)(cpn("a"))
      val digestOther = ProjectDigest.computeAll(otherIndex, buildPaths)(cpn("a"))

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

      val th = the[BleepException] thrownBy ProjectDigest.computeAll(build, buildPaths)
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

      val digestWithout = ProjectDigest.computeAll(noResolvers, buildPaths)(cpn("a"))
      val digestWith = ProjectDigest.computeAll(withResolvers, buildPaths)(cpn("a"))

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

      val digest1 = ProjectDigest.computeAll(build, buildPaths)(cpn("a"))

      Files.writeString(resDir.resolve("app.conf"), "key=value2")

      val digest2 = ProjectDigest.computeAll(build, buildPaths)(cpn("a"))

      digest1 should not be digest2
    } finally deleteRecursively(workspace)
  }
}
