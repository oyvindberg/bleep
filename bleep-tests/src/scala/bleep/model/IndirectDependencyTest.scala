package bleep.model

import bleep.BleepException
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.SortedSet

class IndirectDependencyTest extends AnyFunSuite with Matchers {
  private def name(n: String): CrossProjectName = CrossProjectName(ProjectName(n), None)

  private def build(projects: (String, Project)*): Build.Exploded =
    Build.Exploded(
      $version = BleepVersion("1.0.0-M9"),
      explodedProjects = projects.map { case (n, p) => name(n) -> p }.toMap,
      resolvers = JsonList.empty,
      jvm = None,
      scripts = Map.empty,
      remoteCache = None
    )

  private val scala3 =
    Scala(
      version = Some(VersionScala.Scala3),
      options = Options.empty,
      setup = None,
      compilerPlugins = JsonSet.empty,
      strict = None,
      skipStdlib = None,
      compilerProject = None,
      sbtPlugin = None
    )

  /** One project using every kind of indirect dependency, each on its own project. */
  private val usesAllKinds: Project =
    Project.empty.copy(
      dependsOn = JsonSet(ProjectRef(ProjectName("lib"))),
      sourcegen = JsonSet(SortedSet[ScriptDef](ScriptDef.Main(name("gen"), "gen.Main", JsonSet.empty, JsonSet(name("genInput")), None))),
      scala = Some(scala3.copy(compilerProject = Some(name("compiler")))),
      postCompile = Some(PostCompile(name("post"), "post.Main", JsonSet(name("input"))))
    )

  private val allKinds = build(
    "app" -> usesAllKinds,
    "lib" -> Project.empty,
    "gen" -> Project.empty,
    "genInput" -> Project.empty,
    "compiler" -> Project.empty,
    "post" -> Project.empty,
    "input" -> Project.empty
  )

  test("every kind of indirect dependency is listed, with its reason") {
    allKinds.resolvedIndirectDependencies(name("app")) shouldBe List(
      IndirectDependency(name("gen"), IndirectDependency.Reason.Sourcegen("gen.Main")),
      IndirectDependency(name("genInput"), IndirectDependency.Reason.SourcegenInput("gen.Main")),
      IndirectDependency(name("compiler"), IndirectDependency.Reason.ScalaCompiler),
      IndirectDependency(name("post"), IndirectDependency.Reason.PostCompileScript("post.Main")),
      IndirectDependency(name("input"), IndirectDependency.Reason.PostCompileInput)
    )
  }

  test("build order is dependsOn plus every indirect dependency; the classpath graph is dependsOn alone") {
    allKinds.resolvedBuildOrderDeps(name("app")) shouldBe SortedSet(name("lib"), name("gen"), name("genInput"), name("compiler"), name("post"), name("input"))
    allKinds.resolvedDependsOn(name("app")) shouldBe SortedSet(name("lib"))
  }

  test("the build order closure follows indirect edges of indirect dependencies") {
    // the compiler project is itself post-compiled; its script depends on a library
    val b = build(
      "app" -> Project.empty.copy(scala = Some(scala3.copy(compilerProject = Some(name("compiler"))))),
      "compiler" -> Project.empty.copy(postCompile = Some(PostCompile(name("post"), "post.Main", JsonSet.empty))),
      "post" -> Project.empty.copy(dependsOn = JsonSet(ProjectRef(ProjectName("asm")))),
      "asm" -> Project.empty
    )
    b.transitiveBuildOrderDepsFor(name("app")) shouldBe Set(name("compiler"), name("post"), name("asm"))
    b.transitiveDependenciesFor(name("app")) shouldBe empty
  }

  test("a cycle through an indirect edge fails loudly, naming the cycle") {
    // the compiler of `lib` is built by a project that depends on `lib`
    val b = build(
      "lib" -> Project.empty.copy(scala = Some(scala3.copy(compilerProject = Some(name("compiler"))))),
      "compiler" -> Project.empty.copy(dependsOn = JsonSet(ProjectRef(ProjectName("lib"))))
    )
    val e = intercept[BleepException.Text](b.resolvedBuildOrderDeps)
    e.getMessage should include("build order cycle")
    e.getMessage should (include("lib -> compiler -> lib") or include("compiler -> lib -> compiler"))
  }

  test("a project may not be its own post-compile input") {
    val b = build("lib" -> Project.empty.copy(postCompile = Some(PostCompile(name("post"), "post.Main", JsonSet(name("lib"))))), "post" -> Project.empty)
    intercept[BleepException.Text](b.resolvedBuildOrderDeps).getMessage should include("lib -> lib")
  }

  test("an indirect reference to a project that does not exist fails, naming the project and why it was referenced") {
    val b = build("app" -> Project.empty.copy(postCompile = Some(PostCompile(name("nope"), "post.Main", JsonSet.empty))))
    val e = intercept[BleepException.Text](b.resolvedIndirectDependencies)
    e.getMessage should include("nope")
    e.getMessage should include("PostCompileScript")
  }

  test("mapIndirectReferences rewrites every kind, and nothing else") {
    val renamed = usesAllKinds.mapIndirectReferences(cn => CrossProjectName(ProjectName(cn.name.value + "2"), cn.crossId))
    renamed.indirectReferences.map(_.project.name.value) shouldBe List("gen2", "genInput2", "compiler2", "post2", "input2")
    renamed.dependsOn shouldBe usesAllKinds.dependsOn
  }
}
