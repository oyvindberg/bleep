package bleep

import bleep.commands.RemoteCache
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Which projects the remote cache leaves out, until it is decided how they should be cached. */
class RemoteCacheUnsupportedTest extends AnyFunSuite with Matchers {
  private def cn(n: String) = model.CrossProjectName(model.ProjectName(n), None)

  private val scala3 = model.Scala(
    version = Some(model.VersionScala.Scala3),
    options = model.Options.empty,
    setup = None,
    compilerPlugins = model.JsonSet.empty,
    strict = None,
    skipStdlib = None,
    compilerProject = None
  )

  private val build = model.Build.Exploded(
    $version = model.BleepVersion("1.0.0-M9"),
    explodedProjects = Map(
      cn("plain") -> model.Project.empty,
      cn("lib") -> model.Project.empty.copy(postCompile = Some(model.PostCompile(cn("post"), "post.Main", model.JsonSet.empty))),
      cn("consumer") -> model.Project.empty.copy(dependsOn = model.JsonSet(model.ProjectRef(model.ProjectName("lib")))),
      cn("selfCompiled") -> model.Project.empty.copy(scala = Some(scala3.copy(compilerProject = Some(cn("plain"))))),
      cn("post") -> model.Project.empty
    ),
    resolvers = model.JsonList.empty,
    jvm = None,
    scripts = Map.empty,
    remoteCache = None
  )

  test("projects that need a decision are left out, each with its reason; everything else is cached") {
    RemoteCache.unsupportedReason(build, cn("plain")) shouldBe None
    RemoteCache.unsupportedReason(build, cn("post")) shouldBe None
    RemoteCache.unsupportedReason(build, cn("lib")).get should include("postCompile")
    RemoteCache.unsupportedReason(build, cn("consumer")).get should include("lib")
    RemoteCache.unsupportedReason(build, cn("selfCompiled")).get should include("compilerProject")
  }
}
