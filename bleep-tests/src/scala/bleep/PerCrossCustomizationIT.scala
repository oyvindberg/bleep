package bleep

/** Demonstrates that anything settable at the project level can be overridden per cross variant: deps, scalac options, and source layouts (here `cross-pure`,
  * which lets each platform variant pick up its own dedicated source root in addition to the shared one).
  */
class PerCrossCustomizationIT extends IntegrationTestHarness {
  integrationTest("per-cross deps, options, and source layout") { ws =>
    ws.yaml(
      snippet = "per-cross-customization/bleep.yaml",
      content = s"""projects:
                   |  mylib:
                   |    source-layout: cross-pure
                   |    cross:
                   |      jvm213:
                   |        scala:
                   |          version: ${model.VersionScala.Scala213.scalaVersion}
                   |          options: -Ymacro-annotations
                   |        dependencies:
                   |          - org.typelevel::cats-core:2.10.0
                   |      jvm3:
                   |        scala:
                   |          version: ${model.VersionScala.Scala3.scalaVersion}
                   |          options: -Yretain-trees
                   |    platform:
                   |      name: jvm
                   |""".stripMargin
    )

    ws.file(
      "mylib/src/scala/mylib/Greeting.scala",
      snippet = "per-cross-customization/Greeting.scala",
      content = """package mylib
                  |
                  |object Greeting {
                  |  def hello(name: String): String = s"Hello, $name!"
                  |}
                  |""".stripMargin
    )

    // cross-pure layout: jvm-only file goes under src/scala.jvm/. Same path under both jvm213 and jvm3.
    ws.file(
      "mylib/src/scala.jvm/mylib/JvmOnly.scala",
      snippet = "per-cross-customization/JvmOnly.scala",
      content = """package mylib
                  |
                  |object JvmOnly {
                  |  def info: String = "running on the JVM"
                  |}
                  |""".stripMargin
    )

    ws.compileAll()
    val (started, _, _) = ws.start()
    val crossIds = started.build.explodedProjects.keys.toList.flatMap(_.crossId).map(_.value).distinct.sorted
    assert(crossIds == List("jvm213", "jvm3"))

    // The jvm213 variant resolves cats-core (declared per-cross), jvm3 doesn't.
    val cats213 = started.resolvedProjects(model.CrossProjectName(model.ProjectName("mylib"), Some(model.CrossId("jvm213")))).forceGet("test")
    val cats3 = started.resolvedProjects(model.CrossProjectName(model.ProjectName("mylib"), Some(model.CrossId("jvm3")))).forceGet("test")
    assert(cats213.classpath.exists(_.toString.contains("cats-core_2.13")), "jvm213 should have cats-core_2.13")
    assert(!cats3.classpath.exists(_.toString.contains("cats-core")), "jvm3 should NOT have cats-core")

    // Each variant sees its own scalac option.
    def scalaOptions(rp: ResolvedProject): List[String] = rp.language match {
      case s: ResolvedProject.Language.Scala => s.options
      case _                                 => Nil
    }
    assert(scalaOptions(cats213).contains("-Ymacro-annotations"), "jvm213 missing -Ymacro-annotations")
    assert(scalaOptions(cats3).contains("-Yretain-trees"), "jvm3 missing -Yretain-trees")
  }
}
