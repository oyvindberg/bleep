package bleep.analysis

import bleep.bsp.TaskDag
import bleep.bsp.protocol.{BleepBspProtocol, OutputChannel, TestStatus}
import bleep.testing.TestProtocol
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.concurrent.TimeLimits
import org.scalatest.time.{Seconds, Span}

/** Integration tests for JVM test runner infrastructure.
  *
  * Tests the protocol, data structures, and coordination used for JVM test execution.
  *
  * Note: Full edge case tests (System.exit, OOME, infinite loops) require the ForkedTestRunner to be on classpath. Those tests would verify:
  *   - Timeout handling
  *   - Cancellation
  *   - Process crash handling
  *   - Proper event reporting
  *
  * TODO: Add BspTestHarness for true end-to-end BSP protocol testing.
  */
class JvmTestRunnerIntegrationTest extends AnyFunSuite with Matchers with TimeLimits {

  val quickTimeout: Span = Span(5, Seconds)

  // ==========================================================================
  // TestProtocol Encoding/Decoding Tests
  // ==========================================================================

  test("TestProtocol: encodes and decodes Ready response") {
    failAfter(quickTimeout) {
      val response = TestProtocol.TestResponse.Ready
      val encoded = TestProtocol.encodeResponse(response)
      val decoded = TestProtocol.decodeResponse(encoded)

      decoded shouldBe Right(TestProtocol.TestResponse.Ready)
      encoded should include("Ready")
    }
  }

  test("TestProtocol: encodes and decodes TestStarted response") {
    failAfter(quickTimeout) {
      val response = TestProtocol.TestResponse.TestStarted(
        suite = "com.example.MySuite",
        test = "should add numbers"
      )
      val encoded = TestProtocol.encodeResponse(response)
      val decoded = TestProtocol.decodeResponse(encoded)

      decoded shouldBe Right(response)
      encoded should include("TestStarted")
      encoded should include("MySuite")
    }
  }

  test("TestProtocol: encodes and decodes TestFinished with all fields") {
    failAfter(quickTimeout) {
      val response = TestProtocol.TestResponse.TestFinished(
        suite = "com.example.MySuite",
        test = "should fail gracefully",
        status = "failed",
        durationMs = 123,
        message = Some("expected 1 but got 2"),
        throwable = Some("java.lang.AssertionError: expected 1 but got 2\n\tat MySuite.test(MySuite.scala:10)")
      )
      val encoded = TestProtocol.encodeResponse(response)
      val decoded = TestProtocol.decodeResponse(encoded)

      decoded shouldBe Right(response)
      encoded should include("TestFinished")
      encoded should include("failed")
    }
  }

  test("TestProtocol: encodes and decodes TestFinished without optional fields") {
    failAfter(quickTimeout) {
      val response = TestProtocol.TestResponse.TestFinished(
        suite = "MySuite",
        test = "simple test",
        status = "passed",
        durationMs = 5,
        message = None,
        throwable = None
      )
      val encoded = TestProtocol.encodeResponse(response)
      val decoded = TestProtocol.decodeResponse(encoded)

      decoded shouldBe Right(response)
    }
  }

  test("TestProtocol: encodes and decodes SuiteDone response") {
    failAfter(quickTimeout) {
      val response = TestProtocol.TestResponse.SuiteDone(
        suite = "com.example.MySuite",
        passed = 10,
        failed = 2,
        skipped = 1,
        ignored = 0,
        durationMs = 5432
      )
      val encoded = TestProtocol.encodeResponse(response)
      val decoded = TestProtocol.decodeResponse(encoded)

      decoded shouldBe Right(response)
      encoded should include("SuiteDone")
    }
  }

  test("TestProtocol: encodes and decodes Log response") {
    failAfter(quickTimeout) {
      val response = TestProtocol.TestResponse.Log(
        level = "info",
        message = "Starting test execution...",
        suite = Some("MySuite")
      )
      val encoded = TestProtocol.encodeResponse(response)
      val decoded = TestProtocol.decodeResponse(encoded)

      decoded shouldBe Right(response)
    }
  }

  test("TestProtocol: encodes and decodes Error response") {
    failAfter(quickTimeout) {
      val response = TestProtocol.TestResponse.Error(
        message = "Suite not found: com.example.MissingSuite",
        throwable = Some("java.lang.ClassNotFoundException: com.example.MissingSuite")
      )
      val encoded = TestProtocol.encodeResponse(response)
      val decoded = TestProtocol.decodeResponse(encoded)

      decoded shouldBe Right(response)
      encoded should include("Error")
    }
  }

  test("TestProtocol: encodes and decodes ThreadDump response") {
    failAfter(quickTimeout) {
      val response = TestProtocol.TestResponse.ThreadDump(
        threads = List(
          TestProtocol.TestResponse.ThreadInfo(
            name = "main",
            state = "RUNNABLE",
            stackTrace = List(
              "at java.lang.Thread.sleep(Native Method)",
              "at com.example.MySuite.slowTest(MySuite.scala:25)"
            )
          ),
          TestProtocol.TestResponse.ThreadInfo(
            name = "ForkJoinPool-1-worker-1",
            state = "WAITING",
            stackTrace = List(
              "at sun.misc.Unsafe.park(Native Method)"
            )
          )
        )
      )
      val encoded = TestProtocol.encodeResponse(response)
      val decoded = TestProtocol.decodeResponse(encoded)

      decoded shouldBe Right(response)
      encoded should include("ThreadDump")
    }
  }

  test("TestProtocol: handles malformed JSON gracefully") {
    failAfter(quickTimeout) {
      val malformed = "this is not json"
      val decoded = TestProtocol.decodeResponse(malformed)

      decoded.isLeft shouldBe true
    }
  }

  test("TestProtocol: handles unknown response type gracefully") {
    failAfter(quickTimeout) {
      val unknown = """{"type":"UnknownResponseType","data":{}}"""
      val decoded = TestProtocol.decodeResponse(unknown)

      decoded.isLeft shouldBe true
    }
  }

  test("TestProtocol: handles empty JSON object") {
    failAfter(quickTimeout) {
      val empty = "{}"
      val decoded = TestProtocol.decodeResponse(empty)

      decoded.isLeft shouldBe true
    }
  }

  // ==========================================================================
  // TestCommand Encoding/Decoding Tests
  // ==========================================================================

  test("TestProtocol: encodes and decodes RunSuite command") {
    failAfter(quickTimeout) {
      val command = TestProtocol.TestCommand.RunSuite(
        className = "com.example.MySuite",
        framework = "munit",
        args = List("--verbose", "--include=fast")
      )
      val encoded = TestProtocol.encodeCommand(command)
      val decoded = TestProtocol.decodeCommand(encoded)

      decoded shouldBe Right(command)
      encoded should include("RunSuite")
      encoded should include("munit")
    }
  }

  test("TestProtocol: encodes and decodes RunSuite with empty args") {
    failAfter(quickTimeout) {
      val command = TestProtocol.TestCommand.RunSuite(
        className = "MySuite",
        framework = "scalatest",
        args = Nil
      )
      val encoded = TestProtocol.encodeCommand(command)
      val decoded = TestProtocol.decodeCommand(encoded)

      decoded shouldBe Right(command)
    }
  }

  test("TestProtocol: encodes and decodes Shutdown command") {
    failAfter(quickTimeout) {
      val command = TestProtocol.TestCommand.Shutdown
      val encoded = TestProtocol.encodeCommand(command)
      val decoded = TestProtocol.decodeCommand(encoded)

      decoded shouldBe Right(command)
      encoded should include("Shutdown")
    }
  }

  test("TestProtocol: encodes and decodes GetThreadDump command") {
    failAfter(quickTimeout) {
      val command = TestProtocol.TestCommand.GetThreadDump
      val encoded = TestProtocol.encodeCommand(command)
      val decoded = TestProtocol.decodeCommand(encoded)

      decoded shouldBe Right(command)
      encoded should include("GetThreadDump")
    }
  }

  // ==========================================================================
  // TaskResult Tests
  // ==========================================================================

  test("TaskResult.Success is distinct value") {
    failAfter(quickTimeout) {
      val success = TaskDag.TaskResult.Success
      success shouldBe TaskDag.TaskResult.Success
    }
  }

  test("TaskResult.Failure contains error message and diagnostics") {
    failAfter(quickTimeout) {
      val failure = TaskDag.TaskResult.Failure(
        error = "3 test(s) failed in MySuite",
        diagnostics = List(
          BleepBspProtocol.Diagnostic.error("testAddition: expected 4 but got 5"),
          BleepBspProtocol.Diagnostic.error("testSubtraction: expected 2 but got 3"),
          BleepBspProtocol.Diagnostic.error("testMultiplication: expected 6 but got 8")
        )
      )

      failure.error should include("3 test(s) failed")
      failure.diagnostics should have size 3
      failure.diagnostics.head.message should include("testAddition")
    }
  }

  test("TaskResult.Cancelled is distinct from Failure") {
    failAfter(quickTimeout) {
      val cancelled = TaskDag.TaskResult.Cancelled
      val failure = TaskDag.TaskResult.Failure("error", Nil)

      cancelled should not be failure
      (cancelled == TaskDag.TaskResult.Cancelled) shouldBe true
    }
  }

  // ==========================================================================
  // DagEvent Tests
  // ==========================================================================

  test("DagEvent.TestStarted captures all required fields") {
    failAfter(quickTimeout) {
      val ts = System.currentTimeMillis()
      val event = TaskDag.DagEvent.TestStarted(
        project = "my-project",
        suite = "com.example.MySuite",
        test = "should handle edge cases",
        timestamp = ts
      )

      event.project shouldBe "my-project"
      event.suite shouldBe "com.example.MySuite"
      event.test shouldBe "should handle edge cases"
      event.timestamp shouldBe ts
    }
  }

  test("DagEvent.TestFinished captures status and timing") {
    failAfter(quickTimeout) {
      val ts = System.currentTimeMillis()
      val event = TaskDag.DagEvent.TestFinished(
        project = "my-project",
        suite = "MySuite",
        test = "myTest",
        status = TestStatus.Failed,
        durationMs = 150,
        message = Some("assertion failed: expected true"),
        throwable = Some("java.lang.AssertionError"),
        timestamp = ts
      )

      event.status shouldBe TestStatus.Failed
      event.durationMs shouldBe 150
      event.message shouldBe Some("assertion failed: expected true")
      event.throwable shouldBe Some("java.lang.AssertionError")
    }
  }

  test("DagEvent.SuiteFinished captures counts and duration") {
    failAfter(quickTimeout) {
      val ts = System.currentTimeMillis()
      val event = TaskDag.DagEvent.SuiteFinished(
        project = "my-project",
        suite = "MySuite",
        passed = 10,
        failed = 2,
        skipped = 1,
        ignored = 0,
        durationMs = 5000,
        timestamp = ts
      )

      event.passed shouldBe 10
      event.failed shouldBe 2
      event.skipped shouldBe 1
      event.ignored shouldBe 0
      event.durationMs shouldBe 5000
    }
  }

  test("DagEvent.Output distinguishes stdout from stderr") {
    failAfter(quickTimeout) {
      val ts = System.currentTimeMillis()

      val stdout = TaskDag.DagEvent.Output(
        project = "my-project",
        suite = "MySuite",
        line = "[info] Running tests...",
        channel = OutputChannel.Stdout,
        timestamp = ts
      )

      val stderr = TaskDag.DagEvent.Output(
        project = "my-project",
        suite = "MySuite",
        line = "[error] Test failed!",
        channel = OutputChannel.Stderr,
        timestamp = ts
      )

      stdout.channel shouldBe OutputChannel.Stdout
      stderr.channel shouldBe OutputChannel.Stderr
    }
  }

  // ==========================================================================
  // Protocol Robustness Tests
  // ==========================================================================

  test("TestProtocol: handles special characters in messages") {
    failAfter(quickTimeout) {
      val response = TestProtocol.TestResponse.Log(
        level = "info",
        message = """Line with "quotes" and \backslash and unicode: 日本語""",
        suite = Some("MySuite")
      )
      val encoded = TestProtocol.encodeResponse(response)
      val decoded = TestProtocol.decodeResponse(encoded)

      decoded shouldBe Right(response)
    }
  }

  test("TestProtocol: handles very long messages") {
    failAfter(quickTimeout) {
      val longMessage = "x" * 10000
      val response = TestProtocol.TestResponse.Log(
        level = "debug",
        message = longMessage,
        suite = None
      )
      val encoded = TestProtocol.encodeResponse(response)
      val decoded = TestProtocol.decodeResponse(encoded)

      decoded shouldBe Right(response)
    }
  }

  test("TestProtocol: handles newlines in stack traces") {
    failAfter(quickTimeout) {
      val stackTrace = """java.lang.RuntimeException: boom
        |  at MySuite.test(MySuite.scala:10)
        |  at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
        |  at java.lang.reflect.Method.invoke(Method.java:498)
        |Caused by: java.io.IOException: file not found
        |  at java.io.FileInputStream.<init>(FileInputStream.java:123)""".stripMargin

      val response = TestProtocol.TestResponse.TestFinished(
        suite = "MySuite",
        test = "test",
        status = "error",
        durationMs = 10,
        message = Some("boom"),
        throwable = Some(stackTrace)
      )
      val encoded = TestProtocol.encodeResponse(response)
      val decoded = TestProtocol.decodeResponse(encoded)

      decoded shouldBe Right(response)
    }
  }

  // ==========================================================================
  // Test Status Values
  // ==========================================================================

  test("TestProtocol: all standard status values are valid") {
    failAfter(quickTimeout) {
      val statuses = List("passed", "failed", "error", "skipped", "ignored", "cancelled", "pending")

      for (status <- statuses) {
        val response = TestProtocol.TestResponse.TestFinished(
          suite = "MySuite",
          test = "test",
          status = status,
          durationMs = 10,
          message = None,
          throwable = None
        )
        val encoded = TestProtocol.encodeResponse(response)
        val decoded = TestProtocol.decodeResponse(encoded)

        decoded shouldBe Right(response)
        info(s"Status '$status' encodes/decodes correctly")
      }
    }
  }
}
