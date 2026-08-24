package bleep

/** The heap a forked test JVM actually runs with.
  *
  * Every assertion is made from *inside* the fork, against `Runtime.maxMemory`, because that is the only number that cannot be wrong: the options bleep
  * assembles are evidence of intent, not of what the JVM did with them. This suite exists because a plausible-looking experiment once "proved" that
  * `platform.jvmOptions` never reached the fork at all — it had set `-Xmx3m`, expecting a JVM that refuses to start, and HotSpot quietly rounded it up to a
  * heap the suite ran fine in. Nothing in the build or the CLI reported the number, so there was no way to tell.
  *
  * [[IntegrationTestHarness.testConfig]] sets `testRunnerHeap` to 512m, which is the configured default these tests are measured against.
  */
class TestHeapPropagationIT extends IntegrationTestHarness {

  /** The suite asserts its own heap, so a fork started with the wrong `-Xmx` fails the test rather than quietly passing. Ranges, not equality: `maxMemory` is
    * what the GC made of `-Xmx`, which is the same order but not always the same number.
    */
  private def suiteExpectingHeapMb(loMb: Long, hiMb: Long): String =
    s"""package example
       |
       |import org.scalatest.funsuite.AnyFunSuite
       |
       |class HeapTest extends AnyFunSuite {
       |  test("the forked test jvm runs with the heap the build asked for") {
       |    val maxMb = Runtime.getRuntime.maxMemory / (1024 * 1024)
       |    assert(maxMb >= ${loMb}L && maxMb <= ${hiMb}L, s"expected a heap in [$loMb, $hiMb] MB, got $${maxMb}MB")
       |  }
       |}
       |""".stripMargin

  private def yamlWithJvmOptions(jvmOptions: Option[String]): String =
    s"""projects:
       |  mytest:
       |    dependencies: org.scalatest::scalatest:3.2.15
       |    isTestProject: true
       |    platform:
       |      name: jvm${jvmOptions.fold("")(opts => s"\n      jvmOptions: $opts")}
       |    scala:
       |      version: 3.3.3
       |""".stripMargin

  private val mytest = model.CrossProjectName(model.ProjectName("mytest"), None)

  /** The default a fork gets here when the build states nothing.
    *
    * Read from the same place the server reads it — the user config on disk (`MultiWorkspaceBspServer` loads it fresh per request), NOT
    * [[IntegrationTestHarness.testConfig]], which the in-process server does not consult for this. Computing it rather than hardcoding it is what keeps these
    * assertions true on a developer machine that has set `testRunnerHeap` and on CI, which has no config file at all.
    */
  private val configuredDefaultMb: Long =
    MachineResources.forkHeapMb(BleepConfigOps.loadOrDefault(userPaths).orThrow.bspServerConfigOrDefault.testRunnerHeap)

  private def runTests(started: Started, jvmOptions: List[String]): Either[BleepException, Unit] =
    commands.ReactiveBsp
      .test(
        watch = false,
        projects = Array(mytest),
        displayMode = commands.DisplayMode.NoTui,
        jvmOptions = jvmOptions,
        testArgs = Nil,
        only = Nil,
        exclude = Nil,
        includeTags = Nil,
        excludeTags = Nil,
        flamegraph = false,
        cancel = false,
        junitReportDir = None,
        diffBase = None,
        diffOutput = OutputMode.Text,
        clientEnv = Map.empty
      )
      .run(started)

  integrationTest("platform.jvmOptions -Xmx is the heap the fork runs with") { ws =>
    ws.yaml(yamlWithJvmOptions(Some("-Xmx256m")))
    ws.file("mytest/src/scala/HeapTest.scala", suiteExpectingHeapMb(loMb = 180, hiMb = 320))
    val (started, _, _) = ws.start()
    runTests(started, Nil).orThrow
    succeed
  }

  integrationTest("a project that states nothing gets the configured testRunnerHeap") { ws =>
    ws.yaml(yamlWithJvmOptions(None))
    ws.file("mytest/src/scala/HeapTest.scala", suiteExpectingHeapMb(loMb = configuredDefaultMb * 8 / 10, hiMb = configuredDefaultMb))
    val (started, _, _) = ws.start()
    runTests(started, Nil).orThrow
    succeed
  }

  integrationTest("a project may ask for MORE heap than testRunnerHeap — it is a default, not a ceiling") {
    // The load-bearing case for the whole design: the project asks for more than the machine-level
    // default and must get it. If this ever fails, a machine setting has started overriding what the
    // build says its own code needs, and the only fix would live outside the repo.
    //
    // Derived from the configured default rather than a fixed number, so it is genuinely "more than
    // the default" wherever it runs. `-Xmx` is a ceiling and nothing is committed up front, so asking
    // for a large one costs a reservation, not the memory.
    ws =>
      val requestedMb = configuredDefaultMb + 512
      ws.yaml(yamlWithJvmOptions(Some(s"-Xmx${requestedMb}m")))
      ws.file("mytest/src/scala/HeapTest.scala", suiteExpectingHeapMb(loMb = requestedMb * 9 / 10, hiMb = requestedMb))
      val (started, _, _) = ws.start()
      runTests(started, Nil).orThrow
      succeed
  }

  integrationTest("--jvm-opt on the command line outranks the project's own -Xmx") { ws =>
    ws.yaml(yamlWithJvmOptions(Some("-Xmx256m")))
    ws.file("mytest/src/scala/HeapTest.scala", suiteExpectingHeapMb(loMb = 600, hiMb = 900))
    val (started, _, _) = ws.start()
    runTests(started, List("-Xmx768m")).orThrow
    succeed
  }

  integrationTest("the assertion is load-bearing: a wrong heap fails the suite") {
    // Without this, every test above would still pass if the fork ignored `-Xmx` entirely and ran on
    // some large ambient default — which is the exact failure this suite exists to catch.
    ws =>
      ws.yaml(yamlWithJvmOptions(Some("-Xmx256m")))
      ws.file("mytest/src/scala/HeapTest.scala", suiteExpectingHeapMb(loMb = 4096, hiMb = 8192))
      val (started, _, _) = ws.start()
      assert(runTests(started, Nil).isLeft, "a suite asserting a heap the fork was not given must fail")
      succeed
  }
}
