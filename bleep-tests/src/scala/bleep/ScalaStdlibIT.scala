package bleep

class ScalaStdlibIT extends IntegrationTestHarness {
  // Scala 3.8 introduces a major change: the standard library is now compiled with Scala 3
  // instead of Scala 2.13. This test verifies that bleep can handle Scala 3.8 projects correctly,
  // including the new scala-library versioning (3.x instead of 2.13.x).
  // See: https://docs.scala-lang.org/sips/drop-stdlib-forwards-bin-compat.html
  //
  // Verifies via compile + classpath inspection rather than `commands.run`. The resolution
  // (the actual thing being tested) is fully exercised by compile; forking a 3.8 runtime JVM
  // just to prove `println` works adds substantial memory pressure (the new stdlib is ~9MB
  // vs 2.13's ~5.6MB, plus extra TASTy/classloader overhead) and doesn't add test coverage.
  integrationTest("scala 3.8.1 with new stdlib") { ws =>
    ws.yaml(
      """projects:
        |  a:
        |    platform:
        |      name: jvm
        |      mainClass: test.Main
        |    scala:
        |      version: 3.8.1
        |""".stripMargin
    )
    ws.file(
      "a/src/scala/Main.scala",
      """package test
        |object Main {
        |  def main(args: Array[String]): Unit = {
        |    val list = List(1, 2, 3).map(_ * 2)
        |    println("result: " + list.sum)
        |  }
        |}""".stripMargin
    )
    val (started, commands, _) = ws.start()
    val projectName = model.CrossProjectName(model.ProjectName("a"), None)
    commands.compile(List(projectName))

    val resolved = started.resolvedProjects(projectName).forceGet("test")
    val classpath = resolved.classpath.map(_.toString)
    assert(
      classpath.exists(p => p.contains("scala-library") && p.contains("3.8.1")),
      s"expected scala-library-3.8.1 on classpath for scala 3.8.1 project; got:\n${classpath.mkString("\n")}"
    )
    assert(
      classpath.exists(p => p.contains("scala3-library_3") && p.contains("3.8.1")),
      s"expected scala3-library_3-3.8.1 on classpath; got:\n${classpath.mkString("\n")}"
    )
  }
}
