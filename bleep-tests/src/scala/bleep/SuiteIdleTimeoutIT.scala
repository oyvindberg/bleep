package bleep

/** Two things at once, because neither can be shown without the other.
  *
  * The suite idle timeout counts test events — `TestStarted`, `TestFinished` — and deliberately not output. A suite that prints while finishing no test is idle
  * by that definition and gets killed, which is the behaviour this pins: it is a decision, not an oversight, and it is easy to "fix" by accident.
  *
  * Showing it needs a timeout short enough to test, which needs the harness's config to actually reach the server. It did not until the in-process server was
  * given a `configOverride`: `handleTest` re-read the developer's own `config.yaml`, so this suite's one-minute timeout was silently whatever that file said,
  * and this test passed against a daemon that had never heard of it.
  */
class SuiteIdleTimeoutIT extends IntegrationTestHarness {

  /** One minute, the smallest the setting allows. The fixture below then talks past it without finishing a test. */
  override protected def testConfig: model.BleepConfig =
    super.testConfig.copy(
      bspServerConfig = super.testConfig.bspServerConfig.map(_.copy(testIdleTimeoutMinutes = Some(1)))
    )

  private val mytest = model.CrossProjectName(model.ProjectName("mytest"), None)

  integrationTest("a suite that logs but finishes no test is killed at the idle timeout") { ws =>
    ws.yaml(
      s"""projects:
         |  mytest:
         |    dependencies:
         |      - org.scalameta::munit:${model.Versions.Munit}
         |    isTestProject: true
         |    platform:
         |      name: jvm
         |    scala:
         |      version: ${model.Versions.Scala3}
         |""".stripMargin
    )
    ws.file(
      "mytest/src/scala/example/TalkativeSuite.scala",
      """package example
        |
        |class TalkativeSuite extends munit.FunSuite {
        |  override def munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(5, "min")
        |
        |  test("talks past the idle timeout without finishing") {
        |    val deadline = System.currentTimeMillis() + 90000L
        |    while (System.currentTimeMillis() < deadline) {
        |      println("still working")
        |      Thread.sleep(2000L)
        |    }
        |  }
        |}
        |""".stripMargin
    )
    val (_, commands, _) = ws.start()

    val outcome =
      try {
        commands.test(List(mytest), watch = false, only = None, exclude = None, includeTags = None, excludeTags = None)
        Right(())
      } catch { case e: BleepException => Left(e) }

    outcome match {
      case Right(()) => fail("the talkative suite ran to completion — the one-minute idle timeout from this suite's config did not reach the server")
      case Left(err) =>
        // Killed, not passed. The message is the run's verdict; the "timed out" wording is what the summary reports for a suite the idle clock stopped.
        assert(
          err.message.toLowerCase.contains("timed out") || err.message.toLowerCase.contains("timeout"),
          s"expected the suite to be stopped by the idle timeout, got: ${err.message}"
        )
        succeed
    }
  }
}
