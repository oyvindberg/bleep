package bleep

/** Maven's `runtime` scope, `configuration: runtime` in bleep: a library there when the code runs, not to compile against. A jdbc driver is the usual one */
class RuntimeScopeIT extends IntegrationTestHarness {

  private def cross(name: String) = model.CrossProjectName(model.ProjectName(name), None)

  integrationTest("a runtime dependency is on the runtime classpath only, travels to consumers as such, and tests compile against it") { ws =>
    ws.yaml(
      """projects:
        |  app:
        |    dependencies:
        |      - configuration: runtime
        |        module: com.h2database:h2:2.2.224
        |      - org.slf4j:slf4j-api:2.0.16
        |    platform:
        |      name: jvm
        |  user:
        |    dependsOn: app
        |    platform:
        |      name: jvm
        |  app-test:
        |    dependsOn: app
        |    isTestProject: true
        |    platform:
        |      name: jvm
        |""".stripMargin
    )
    val (started, _, _) = ws.start()

    def has(project: String, usage: Usage, jar: String): Boolean =
      started.resolvedProject(cross(project)).classpath(usage).exists(_.getFileName.toString.startsWith(jar))

    assert(!has("app", Usage.Compile, "h2-"), "app compiles against its runtime dependency")
    assert(has("app", Usage.Runtime, "h2-"), "app runs without its runtime dependency")
    assert(has("app", Usage.Compile, "slf4j-api-"), "app lost its compile dependency")

    assert(!has("user", Usage.Compile, "h2-"), "a consumer compiles against app's runtime dependency")
    assert(has("user", Usage.Runtime, "h2-"), "a consumer runs without app's runtime dependency")

    assert(has("app-test", Usage.Compile, "h2-"), "tests don't compile against the runtime dependency, which maven's test classpath has")
  }
}
