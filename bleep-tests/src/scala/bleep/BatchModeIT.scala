package bleep

import bleep.commands.{DisplayMode, ReactiveBsp}
import ryddig.{Stored, TypedLogger}

import java.nio.file.{Files, Path}

/** Per-project batch mode (`testFork: per-project`) runs every one of a project's JUnit-Platform suites through a SINGLE `launcher.execute()` in one fork, so
  * an execution-scoped fixture — an application booted for the run — is built once and reused, the way maven surefire's one-execute-per-module works.
  *
  * The whole promise is that nothing downstream can tell the difference: each suite must still report its own result, its own captured output, and real
  * per-test durations, exactly as a suite-by-suite run does — only the fork does one execute instead of N. These tests pin that promise against the JUnit XML
  * the run writes, which is the authoritative per-suite record (see [[JUnitReports]]): a batch that silently merged suites, dropped a sibling's output into the
  * wrong `<system-out>`, or reported `time="0"` for every case would pass a mere "the run was green" check and fail here.
  */
class BatchModeIT extends IntegrationTestHarness {

  private val project = model.CrossProjectName(model.ProjectName("batched-test"), None)

  /** Three JUnit-Platform suites in one project, `testFork: per-project` so they batch into one fork. `maxConcurrentSuites: 2` runs them concurrently, which is
    * the path where per-thread output capture and per-suite attribution have to hold under interleaving.
    */
  private def yamlFor(jupiter: String): String =
    s"""projects:
       |  batched-test:
       |    platform:
       |      name: jvm
       |    isTestProject: true
       |    testFork: per-project
       |    maxConcurrentSuites: 2
       |    dependencies:
       |      - org.junit.jupiter:junit-jupiter:$jupiter
       |""".stripMargin

  private val jupiter = "5.10.1"

  /** A suite that prints a suite-specific marker to stdout, records the pid of the JVM it ran in, and passes two tests. Parameterised so three near-identical
    * suites differ only in name and marker — enough to tell their results, output and JVM apart.
    */
  private def suiteSource(name: String, extraTest: String): String =
    s"""package com.example;
       |
       |import static org.junit.jupiter.api.Assertions.assertEquals;
       |import static org.junit.jupiter.api.Assertions.assertTrue;
       |
       |import java.nio.file.Files;
       |import java.nio.file.Path;
       |import org.junit.jupiter.api.Test;
       |
       |class ${name}Test {
       |  @Test
       |  void recordsAndPrints() throws Exception {
       |    System.out.println("MARKER-$name");
       |    Path dir = Path.of(System.getProperty("batch.piddir"));
       |    Files.writeString(dir.resolve("$name"), String.valueOf(ProcessHandle.current().pid()));
       |    assertEquals(2, 1 + 1);
       |  }
       |
       |  @Test
       |  void alsoPasses() {
       |    assertTrue(true);
       |  }
       |$extraTest
       |}
       |""".stripMargin

  /** A test that sleeps a measurable amount, so its `<testcase time>` proves the runner is timing tests itself (JUnit Platform's listener carries no duration —
    * without our own stopwatch every case would be `time="0"`).
    */
  private val slowTest =
    """
      |  @Test
      |  void slow() throws Exception {
      |    Thread.sleep(60);
      |    assertTrue(true);
      |  }
      |""".stripMargin

  private def runBatch(ws: Workspace, pidDir: Path, reportDir: Path): (Started, TypedLogger[Array[Stored]]) = {
    val (started, _, storingLogger) = ws.start()
    Files.createDirectories(pidDir)
    // A run with a failing suite returns Left; the per-suite record is the XML, asserted by the caller.
    val _ = ReactiveBsp
      .test(
        watch = false,
        projects = Array(project),
        displayMode = DisplayMode.NoTui,
        jvmOptions = List(s"-Dbatch.piddir=$pidDir"),
        testArgs = Nil,
        only = Nil,
        exclude = Nil,
        includeTags = Nil,
        excludeTags = Nil,
        flamegraph = false,
        cancel = false,
        junitReportDir = Some(reportDir),
        diffBase = None,
        diffOutput = OutputMode.Text,
        clientEnv = Map.empty
      )
      .run(started)
    (started, storingLogger)
  }

  integrationTest("per-project batch: each suite reports separately, in one JVM, with attributed output and real durations") { ws =>
    ws.yaml(yamlFor(jupiter))
    ws.file("batched-test/src/java/com/example/AlphaTest.java", suiteSource("Alpha", slowTest))
    ws.file("batched-test/src/java/com/example/BetaTest.java", suiteSource("Beta", ""))
    ws.file("batched-test/src/java/com/example/GammaTest.java", suiteSource("Gamma", ""))

    val pidDir = ws.root.resolve("pids")
    val reportDir = ws.root.resolve("junit-reports")
    val (_, storingLogger) = runBatch(ws, pidDir, reportDir)

    val suites = JUnitReports.read(reportDir)
    def suite(name: String): JUnitReports.Suite =
      suites.find(_.name == s"com.example.${name}Test").getOrElse(fail(s"no suite com.example.${name}Test; got ${suites.map(_.name)}"))

    // 1. Per-suite results survive the batch: three distinct suites, each green, none merged into another.
    assert(suites.map(_.name).toSet == Set("com.example.AlphaTest", "com.example.BetaTest", "com.example.GammaTest"), suites.map(_.describe))
    assert(suite("Alpha").passed == 3, suite("Alpha").describe) // recordsAndPrints + alsoPasses + slow
    assert(suite("Beta").passed == 2, suite("Beta").describe)
    assert(suite("Gamma").passed == 2, suite("Gamma").describe)
    assertSuitePassed(storingLogger, "com.example.BetaTest", tests = 2)

    // 2. One fork for the whole project: every suite recorded the same pid.
    val pids = List("Alpha", "Beta", "Gamma").map(n => Files.readString(pidDir.resolve(n)).trim)
    assert(pids.distinct.size == 1, s"expected all suites in one JVM, got pids $pids")

    // 3. Output is attributed to its own suite — Alpha's marker in Alpha's <system-out>, and nowhere else.
    assert(suite("Alpha").systemOut.contains("MARKER-Alpha"), s"Alpha system-out: ${suite("Alpha").systemOut}")
    assert(!suite("Beta").systemOut.contains("MARKER-Alpha"), s"Beta system-out leaked Alpha's marker: ${suite("Beta").systemOut}")
    assert(suite("Beta").systemOut.contains("MARKER-Beta"), s"Beta system-out: ${suite("Beta").systemOut}")

    // 4. Real per-test durations: the 60ms sleeper is timed, not reported as zero.
    val slow = suite("Alpha").cases.find(_.name.startsWith("slow")).getOrElse(fail(s"no slow case in ${suite("Alpha").cases.map(_.name)}"))
    assert(slow.timeSeconds >= 0.03, s"slow test timed as ${slow.timeSeconds}s — durations are not being measured")
  }

  integrationTest("per-project batch: a failing suite does not sink its siblings") { ws =>
    ws.yaml(yamlFor(jupiter))
    ws.file(
      "batched-test/src/java/com/example/AlphaTest.java",
      s"""package com.example;
         |
         |import static org.junit.jupiter.api.Assertions.assertEquals;
         |
         |import org.junit.jupiter.api.Test;
         |
         |class AlphaTest {
         |  @Test
         |  void boom() {
         |    assertEquals("expected", "actual");
         |  }
         |}
         |""".stripMargin
    )
    ws.file("batched-test/src/java/com/example/BetaTest.java", suiteSource("Beta", ""))
    ws.file("batched-test/src/java/com/example/GammaTest.java", suiteSource("Gamma", ""))

    val pidDir = ws.root.resolve("pids")
    val reportDir = ws.root.resolve("junit-reports")
    runBatch(ws, pidDir, reportDir)

    val suites = JUnitReports.read(reportDir)
    def suite(name: String): JUnitReports.Suite =
      suites.find(_.name == s"com.example.${name}Test").getOrElse(fail(s"no suite com.example.${name}Test; got ${suites.map(_.name)}"))

    // Alpha's one test failed and is reported as a failure...
    assert(suite("Alpha").failures + suite("Alpha").errors >= 1, suite("Alpha").describe)
    assert(suite("Alpha").cases.exists(c => c.status == "failure" || c.status == "error"), suite("Alpha").describe)
    // ...while its batch-mates ran to completion and came back green — the failure did not take the shared execute down with it.
    assert(suite("Beta").passed == 2, suite("Beta").describe)
    assert(suite("Gamma").passed == 2, suite("Gamma").describe)
  }
}
