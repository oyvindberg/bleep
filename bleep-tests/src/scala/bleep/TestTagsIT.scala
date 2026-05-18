package bleep

import cats.data.NonEmptyList

/** End-to-end coverage for `testTags` declared in bleep.yaml combined with `--only-tag` / `--exclude-tag` from the CLI / Commands API.
  *
  * Pure filter logic is unit-tested by `TestTagFilterTest`; these tests prove the full pipeline — model decode → CLI/BSP wiring → suite dispatch — actually
  * fires in concert. They also pin the user-facing error messages so that future refactors can't silently degrade them.
  */
class TestTagsIT extends IntegrationTestHarness {

  /** A test project with one IT and one Test class. `**IT` is tagged slow; other classes are untagged. */
  private val TaggedYaml =
    """projects:
      |  mytest:
      |    dependencies: org.scalatest::scalatest:3.2.15
      |    isTestProject: true
      |    platform:
      |      name: jvm
      |    scala:
      |      version: 3.3.3
      |    testTags:
      |      slow: "**IT"
      |""".stripMargin

  private val FastTest =
    """package example
      |
      |import org.scalatest.funsuite.AnyFunSuite
      |
      |class FastTest extends AnyFunSuite {
      |  test("fast") { assert(1 + 1 == 2) }
      |}
      |""".stripMargin

  private val SlowIT =
    """package example
      |
      |import org.scalatest.funsuite.AnyFunSuite
      |
      |class HeavyIT extends AnyFunSuite {
      |  test("slow") { assert(1 + 1 == 2) }
      |}
      |""".stripMargin

  /** A failing IT — we use this to prove tag filtering actually skipped its suite. If filtering broke and the IT ran, the test fails. */
  private val FailingIT =
    """package example
      |
      |import org.scalatest.funsuite.AnyFunSuite
      |
      |class FailingIT extends AnyFunSuite {
      |  test("would fail if it ran") { assert(false, "FailingIT should have been excluded by tag filter") }
      |}
      |""".stripMargin

  private val mytest = model.CrossProjectName(model.ProjectName("mytest"), None)

  integrationTest("--only-tag slow runs only **IT-matching suites, drops untagged ones") { ws =>
    ws.yaml(TaggedYaml)
    ws.file("mytest/src/scala/FastTest.scala", FastTest)
    ws.file("mytest/src/scala/HeavyIT.scala", SlowIT)
    val (_, commands, _) = ws.start()
    // FastTest is untagged; with --only-tag slow it must be excluded. HeavyIT is the only suite that should run.
    commands.test(
      projects = List(mytest),
      watch = false,
      only = None,
      exclude = None,
      includeTags = Some(NonEmptyList.of("slow")),
      excludeTags = None
    )
    succeed
  }

  integrationTest("--exclude-tag slow skips **IT suites, keeps untagged ones running") { ws =>
    ws.yaml(TaggedYaml)
    ws.file("mytest/src/scala/FastTest.scala", FastTest)
    // FailingIT would fail if it ran. Tag filter must drop it before dispatch — that is the assertion.
    ws.file("mytest/src/scala/FailingIT.scala", FailingIT)
    val (_, commands, _) = ws.start()
    commands.test(
      projects = List(mytest),
      watch = false,
      only = None,
      exclude = None,
      includeTags = None,
      excludeTags = Some(NonEmptyList.of("slow"))
    )
    succeed
  }

  integrationTest("--only-tag combined with --only narrows by both: AND semantics") { ws =>
    ws.yaml(TaggedYaml)
    ws.file("mytest/src/scala/FastTest.scala", FastTest)
    ws.file("mytest/src/scala/HeavyIT.scala", SlowIT)
    val (_, commands, _) = ws.start()
    // --only matches HeavyIT; --only-tag slow also accepts HeavyIT. Intersection runs HeavyIT only.
    commands.test(
      projects = List(mytest),
      watch = false,
      only = Some(NonEmptyList.of("HeavyIT")),
      exclude = None,
      includeTags = Some(NonEmptyList.of("slow")),
      excludeTags = None
    )
    succeed
  }

  integrationTest("--only-tag matching nothing in project fails with a clear pipeline message") { ws =>
    ws.yaml(TaggedYaml)
    // Only an untagged suite; --only-tag slow must error since the project declares `slow` but no suite matches the `**IT` pattern.
    ws.file("mytest/src/scala/FastTest.scala", FastTest)
    val (_, commands, storingLogger) = ws.start()
    intercept[BleepException] {
      commands.test(
        projects = List(mytest),
        watch = false,
        only = None,
        exclude = None,
        includeTags = Some(NonEmptyList.of("slow")),
        excludeTags = None
      )
    }
    // BSP returns the detailed message through a Build Error rendered into the logs; the top-level exception just says "Tests failed".
    // The user reads the explanatory line below the summary, so we assert against the log stream.
    val allLogText = storingLogger.underlying.iterator.map(_.message.plainText).mkString("\n")
    assert(allLogText.contains("--only-tag matched no test suites in mytest"), s"expected pipeline header in logs, got:\n$allLogText")
    assert(allLogText.contains("1 discovered"), s"expected discovered count in logs, got:\n$allLogText")
    assert(allLogText.contains("0 after tag filter"), s"expected post-tag-filter count in logs, got:\n$allLogText")
    assert(allLogText.contains("Tags declared in mytest: slow"), s"expected manifest hint in logs, got:\n$allLogText")
  }

  integrationTest("--only-tag for an unknown-in-this-project tag fails informatively") { ws =>
    // CLI strict validation (Argument.fromMap) rejects unknown tags before commands.test is even reached, so here we exercise the path where
    // the build globally declares a tag but THIS particular project doesn't — the project pre-filter would normally drop the project,
    // so the failure surfaces as "No projects to test" rather than a BSP-side error.
    ws.yaml(
      """projects:
        |  mytest:
        |    dependencies: org.scalatest::scalatest:3.2.15
        |    isTestProject: true
        |    platform:
        |      name: jvm
        |    scala:
        |      version: 3.3.3
        |  other:
        |    dependencies: org.scalatest::scalatest:3.2.15
        |    isTestProject: true
        |    platform:
        |      name: jvm
        |    scala:
        |      version: 3.3.3
        |    testTags:
        |      smoke: "**Smoke"
        |""".stripMargin
    )
    ws.file("mytest/src/scala/FastTest.scala", FastTest)
    val (_, commands, _) = ws.start()
    // mytest declares no testTags at all. `--only-tag smoke` pre-filters it out → 0 projects to test (no exception, just an info log).
    commands.test(
      projects = List(mytest),
      watch = false,
      only = None,
      exclude = None,
      includeTags = Some(NonEmptyList.of("smoke")),
      excludeTags = None
    )
    succeed
  }
}
