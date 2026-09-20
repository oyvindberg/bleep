package bleep

import bleep.testing.{FrameworkSelection, InProcessTestExecutor, TestProtocol, TestSessionRequest}
import bleep.bsp.protocol.SuiteOutcome
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite

import java.io.File
import java.nio.file.{Path, Paths}

/** A real ScalaTest suite, run by [[InProcessTestExecutor]], in the JVM running this test.
  *
  * The classpath comes from `java.class.path`, which inside a bleep test fork is the test classpath bleep assembled — so it carries ScalaTest, the sbt test
  * interface, and `bleep-test-runner` itself. That is exactly the shape of classpath the executor is built for, available here without resolving anything.
  *
  * Worth doing at this level rather than asserting on the wiring: the mechanism this covers is a reflective call across a classloader boundary into a
  * `SuiteRunner` loaded from the project's own copy, with only `java.*` types crossing. Every way that can be wrong — the constructor signature not matching,
  * the platform parent hiding something the framework needs, protocol lines not decoding — is invisible to a type checker and shows up only when a framework is
  * actually loaded and asked to run something.
  */
class InProcessTestExecutorTest extends AnyFunSuite {

  private val fixtureFqn = "bleep.InProcessFixtureSuite"

  private def currentClasspath: List[Path] =
    System.getProperty("java.class.path").split(File.pathSeparator).toList.filter(_.nonEmpty).map(Paths.get(_))

  private def runFixture(suite: String): List[TestProtocol.TestResponse] = {
    val executor = new InProcessTestExecutor(maxConcurrentSuites = 2)
    val request = TestSessionRequest(
      label = suite,
      classpath = currentClasspath,
      jvmOptions = Nil,
      defaultHeapMb = 0L,
      runnerClass = "bleep.testing.runner.ForkedTestRunner",
      environment = Map.empty,
      workingDirectory = None,
      sharing = bleep.testing.SessionSharing.Exclusive
    )
    try
      executor
        .acquire(request)
        .use(session => session.runSuite(suite, FrameworkSelection.SbtTestInterface("scalatest", "org.scalatest.tools.Framework"), Nil).compile.toList)
        .unsafeRunSync()
    finally executor.shutdown.unsafeRunSync()
  }

  test("a ScalaTest suite runs in this process and reports every test it executed") {
    val responses = runFixture(fixtureFqn)

    val finished = responses.collect { case f: TestProtocol.TestResponse.TestFinished => f }
    assert(
      finished.map(_.test).sorted == List("addition still works", "strings are still strings"),
      s"expected both fixture tests to report; got:\n${responses.mkString("\n")}"
    )
    assert(finished.forall(_.status == "passed"), s"fixture tests should pass: ${finished.map(f => s"${f.test}=${f.status}")}")

    val done = responses.collect { case d: TestProtocol.TestResponse.SuiteDone => d }
    assert(done.size == 1, s"expected exactly one terminal SuiteDone, got ${done.size}")
    assert(done.head.outcome == SuiteOutcome.Executed(passed = 2, failed = 0, skipped = 0, ignored = 0), s"outcome was ${done.head.outcome}")
  }

  test("a class that cannot be run is reported as errored, never as an empty green suite") {
    // The distinction that matters most in this whole path: a class that produced no test results must not read as a suite that simply had none. ScalaTest
    // does claim `java.lang.String` at fingerprint time and then refuses it at execute time, so the failure arrives as a throwable out of `task.execute` —
    // which is the case `SuiteRunner.executeTasks` deliberately propagates rather than swallowing, because swallowing it reported a suite that never ran as
    // passing. Asserted here on the in-process path for the same reason it matters on the forked one.
    val responses = runFixture("java.lang.String")
    val done = responses.collect { case d: TestProtocol.TestResponse.SuiteDone => d }
    assert(done.size == 1, s"expected exactly one terminal SuiteDone, got:\n${responses.mkString("\n")}")
    done.head.outcome match {
      case SuiteOutcome.Errored(message, _) =>
        assert(message.contains("java.lang.String"), s"the error should name the class it could not run: $message")
      case other =>
        fail(s"a class that ran nothing must not report as $other")
    }
  }

  test("the executor refuses a request it cannot honour rather than running the tests without it") {
    val executor = new InProcessTestExecutor(maxConcurrentSuites = 1)
    val withOptions = TestSessionRequest(
      label = "some-suite",
      classpath = currentClasspath,
      jvmOptions = List("-Xmx4g"),
      defaultHeapMb = 0L,
      runnerClass = "bleep.testing.runner.ForkedTestRunner",
      environment = Map.empty,
      workingDirectory = None,
      sharing = bleep.testing.SessionSharing.Exclusive
    )
    val thrown = intercept[RuntimeException](executor.acquire(withOptions).use(_ => cats.effect.IO.unit).unsafeRunSync())
    assert(thrown.getMessage.contains("-Xmx4g"), s"the refusal should name what it could not honour: ${thrown.getMessage}")
    executor.shutdown.unsafeRunSync()
  }
}

/** The suite [[InProcessTestExecutorTest]] runs through the in-process executor. Only passing tests: it is discovered and run by the outer build as well, being
  * an ordinary suite on this project's test classpath, and a fixture that failed on purpose would fail that run too.
  */
class InProcessFixtureSuite extends AnyFunSuite {
  test("addition still works")(assert(1 + 1 == 2))
  test("strings are still strings")(assert("bleep".nonEmpty))
}
