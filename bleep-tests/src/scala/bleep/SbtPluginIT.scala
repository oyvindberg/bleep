package bleep

class SbtPluginIT extends IntegrationTestHarness {
  integrationTest("an org::name sbt plugin resolves as an sbt 1 plugin for scala 2.12 and an sbt 2 plugin for scala 3") { ws =>
    ws.yaml(
      """projects:
        |  myplugin:
        |    dependencies:
        |      - module: com.github.sbt::sbt-dynver:5.1.1
        |        isSbtPlugin: true
        |    platform:
        |      name: jvm
        |    cross:
        |      jvm212:
        |        scala:
        |          version: 2.12.20
        |      jvm3:
        |        scala:
        |          version: 3.8.4
        |""".stripMargin
    )
    val (started, _, _) = ws.start()
    def jars(crossId: String): List[String] =
      started
        .resolvedProjects(model.CrossProjectName(model.ProjectName("myplugin"), Some(model.CrossId(crossId))))
        .forceGet("test")
        .classpath(Usage.Compile)
        .map(_.getFileName.toString)
        .filter(_.startsWith("sbt-dynver"))
    assert(jars("jvm212") == List("sbt-dynver_2.12_1.0-5.1.1.jar"), jars("jvm212"))
    assert(jars("jvm3") == List("sbt-dynver_sbt2_3-5.1.1.jar"), jars("jvm3"))
  }
}
