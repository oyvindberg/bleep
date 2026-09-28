package bleep
package mavenimport

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Path

class ResolutionReportTest extends AnyFunSuite {
  private def module(artifactId: String) =
    MavenProject(
      "com.example",
      artifactId,
      "1.0",
      "jar",
      Path.of(s"/b/$artifactId"),
      Path.of("src"),
      Path.of("test"),
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Map.empty
    )

  private def cached(group: String, artifact: String, version: String, classifier: String): Path = {
    val suffix = if (classifier.isEmpty) "" else s"-$classifier"
    Path.of(s"/cache/https/repo1.maven.org/maven2/${group.replace('.', '/')}/$artifact/$version/$artifact-$version$suffix.jar")
  }

  test("libraries at another version, only in maven and only in bleep, per project") {
    val mavenResolved = Map(
      "app" -> List(
        MavenResolvedDependency("ch.qos.logback", "logback-core", "jar", "", "1.5.38", "compile"),
        MavenResolvedDependency("org.slf4j", "slf4j-api", "jar", "", "2.0.17", "compile"),
        MavenResolvedDependency("io.netty", "netty-transport-native-epoll", "jar", "linux-x86_64", "4.2.3", "runtime"),
        MavenResolvedDependency("org.junit.jupiter", "junit-jupiter-api", "jar", "", "5.14.0", "test"),
        // a module of the build: a project the others depend on in bleep
        MavenResolvedDependency("com.example", "lib", "jar", "", "1.0", "compile")
      )
    )
    val main = model.CrossProjectName(model.ProjectName("app"), None)
    val test = model.CrossProjectName(model.ProjectName("app-test"), None)
    val classpaths = Map(
      main -> List(
        cached("ch.qos.logback", "logback-core", "1.6.3", ""),
        cached("org.slf4j", "slf4j-api", "2.0.17", ""),
        cached("io.netty", "netty-transport-native-epoll", "4.2.3", "linux-x86_64"),
        cached("com.google.guava", "guava", "33.0", ""),
        Path.of("/b/.bleep/projects/lib/classes")
      ),
      test -> List(
        cached("ch.qos.logback", "logback-core", "1.6.3", ""),
        cached("org.slf4j", "slf4j-api", "2.0.17", ""),
        cached("io.netty", "netty-transport-native-epoll", "4.2.3", "linux-x86_64"),
        cached("com.google.guava", "guava", "33.0", "")
      )
    )
    val report = ResolutionReport(List(module("app"), module("lib")), mavenResolved, classpaths)
    assert(report.compared == 2)
    assert(report.differences(main).map(_.render) == List("+ guava:33.0", "~ logback-core: 1.5.38 -> 1.6.3"))
    assert(report.differences(test).map(_.render) == List("+ guava:33.0", "- junit-jupiter-api:5.14.0", "~ logback-core: 1.5.38 -> 1.6.3"))
    assert(report.mostCommon.head == ("+ guava:33.0", 2))
  }

  test("no differences, no report") {
    val main = model.CrossProjectName(model.ProjectName("app"), None)
    val report = ResolutionReport(
      List(module("app")),
      Map("app" -> List(MavenResolvedDependency("org.slf4j", "slf4j-api", "jar", "", "2.0.17", "compile"))),
      Map(main -> List(cached("org.slf4j", "slf4j-api", "2.0.17", "")))
    )
    assert(report.isEmpty)
  }
}
