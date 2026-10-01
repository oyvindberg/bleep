package bleep

import bleep.internal.FileUtils

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** End-to-end proof that `@QuarkusTest` works in a bleep build, with no Maven or Gradle anywhere.
  *
  * The interesting machinery under test:
  *   - `bleep.plugin.quarkus.QuarkusTestModelGen` (sourcegen) builds the serialized Quarkus application model by forking `bleep.quarkus.QuarkusModelWriter`
  *     with the app's own Quarkus version on the classpath — resolved via the `${BLEEP_VERSION}` dev-deps shim, exactly like `bleep-test-runner`.
  *   - The sourcegen also declares the fork's JVM options (serialized-model path, jboss LogManager) by writing them to the project's `forkJvmOptions` file,
  *     which bleep appends when it assembles the fork — so `template-quarkus-test` carries no `platform` block, and bleep's server has no Quarkus-specific
  *     code.
  *   - Inside the fork, Quarkus bootstraps from the serialized model (its Gradle-plugin escape hatch), augments in-process, starts the HTTP server, and the
  *     JUnit platform runs the suite like any other.
  */
class QuarkusMyappIT extends IntegrationTestHarness {

  integrationTest("quarkus-myapp compiles and runs @QuarkusTest against the augmented application") { ws =>
    // bleep.yaml is authored inline because the harness needs `$version: dev` in the prelude so
    // `build.bleep:bleep-plugin-quarkus:${BLEEP_VERSION}` resolves to the in-memory bleep build via
    // ResolveProjects.ReplaceBleepDependencies (rather than pulling a stale release from Maven Central).
    ws.yaml(
      snippet = "quarkus-myapp/bleep.yaml",
      content = """projects:
                  |  myapp:
                  |    dependencies:
                  |      - io.quarkus:quarkus-rest:3.39.1
                  |    extends: template-common
                  |  myapp-test:
                  |    dependencies:
                  |      - io.quarkus:quarkus-junit5:3.39.1
                  |    dependsOn: myapp
                  |    extends:
                  |      - template-common
                  |      - template-quarkus-test
                  |  scripts:
                  |    dependencies:
                  |      - build.bleep:bleep-plugin-quarkus:${BLEEP_VERSION}
                  |    java:
                  |      options: -proc:none --release 17
                  |    platform:
                  |      name: jvm
                  |scripts:
                  |  run-myapp-dev:
                  |    main: scripts.RunMyappDev
                  |    project: scripts
                  |  package-myapp:
                  |    main: scripts.PackageMyapp
                  |    project: scripts
                  |templates:
                  |  template-common:
                  |    java:
                  |      options: -proc:none --release 17 -parameters
                  |    platform:
                  |      name: jvm
                  |  template-quarkus-test:
                  |    isTestProject: true
                  |    sourcegen:
                  |      main: bleep.plugin.quarkus.QuarkusTestModelGen
                  |      project: scripts
                  |""".stripMargin
    )

    val fixtureRoot = FileUtils.cwd.resolve("docs-snippets-from-tests/quarkus-myapp")
    copyFixtureInto(ws, fixtureRoot, skip = "bleep.yaml")

    val (started, commands, storingLogger) = ws.start()

    // Compiling the test project drives the whole chain: scripts compile, QuarkusTestModelGen runs
    // as sourcegen, the model writer forks, and the .dat lands at the stable path the yaml names.
    commands.compile(List(named("myapp-test")))

    val modelFile = started.projectPaths(named("myapp-test")).targetDir.resolve("quarkus").resolve("test-app-model.dat")
    assert(Files.exists(modelFile), s"expected the sourcegen to have produced $modelFile")

    // The substantive proof: a real @QuarkusTest — CDI wiring, augmentation from the serialized
    // model, an actual HTTP round-trip against the started application.
    commands.test(
      projects = List(named("myapp-test")),
      watch = false,
      only = None,
      exclude = None,
      includeTags = None,
      excludeTags = None
    )

    assertSuitePassed(storingLogger, "com.example.myapp.GreetingResourceTest", tests = 2)
  }

  /** Walks every file under `fixtureRoot` (excluding the file named by `skip`), reads the content, writes it into the workspace at the same relative path, and
    * tags it as a snippet at `quarkus-myapp/<relPath>` so the harness's mirror loop overwrites the fixture with the same bytes (no-op).
    */
  private def copyFixtureInto(ws: Workspace, fixtureRoot: Path, skip: String): Unit =
    Files
      .walk(fixtureRoot)
      .iterator()
      .asScala
      .filter(Files.isRegularFile(_))
      .filterNot(p => p.toString.contains("/.bleep/") || p.toString.contains("/build/"))
      .foreach { src =>
        val relPath = fixtureRoot.relativize(src).toString
        if (relPath != skip) {
          val content = Files.readString(src)
          ws.file(relPath, content, snippet = s"quarkus-myapp/$relPath")
        }
      }

  private def named(name: String): model.CrossProjectName =
    model.CrossProjectName(model.ProjectName(name), None)
}
