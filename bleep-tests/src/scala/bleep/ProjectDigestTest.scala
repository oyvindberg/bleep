package bleep

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.jdk.StreamConverters.*

class ProjectDigestTest extends AnyFunSuite with Matchers {

  private def cpn(name: String): model.CrossProjectName =
    model.CrossProjectName(model.ProjectName(name), None)

  private def projectWithDeps(deps: String*): model.Project =
    model.Project.empty.copy(
      dependsOn = model.JsonSet(SortedSet.from(deps.map(model.ProjectName.apply)))
    )

  private def makeBuild(projects: (String, model.Project)*): model.Build.Exploded =
    model.Build.Exploded(
      $version = model.BleepVersion("test"),
      explodedProjects = projects.map { case (name, p) => cpn(name) -> p }.toMap,
      resolvers = model.JsonList.empty,
      jvm = None,
      scripts = Map.empty
    )

  private def createTempWorkspace(): Path = {
    val dir = Files.createTempDirectory("bleep-digest-test-")
    Files.createDirectories(dir.resolve("src"))
    dir
  }

  private def deleteRecursively(path: Path): Unit =
    if (Files.exists(path)) {
      if (Files.isDirectory(path))
        Files.list(path).toScala(List).foreach(deleteRecursively)
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
      info(s"empty project digest: $digest")
      digest shouldBe "ca3d163bab055381827226140568f3bef7eaac187cebd76878e0b63e9e442356"
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
      digest shouldBe "f3061cdc50db44978246c6b8f6b62093e3d9243f6986890b31cc57bbaa12a406"
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
