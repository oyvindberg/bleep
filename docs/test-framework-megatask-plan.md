# Test Framework Integration Megatask

## Overview

Comprehensive integration test suite for all supported test frameworks × all supported languages. Each combination has 4 test scenarios:
1. **Passing test** - verify success status
2. **Failing assertion** - verify failure detection
3. **Exception thrown** - verify error handling
4. **Spectacular failure** - stack overflow, OOM, deadlock (verify harness recovery)

Special focus on **JUnit 5/Jupiter** which isn't well-supported elsewhere in Scala ecosystem.

---

## Framework × Language Matrix

| Framework | Scala 2.13 | Scala 3 | Java | Kotlin |
|-----------|------------|---------|------|--------|
| ScalaTest | ✓ | ✓ | - | - |
| MUnit | ✓ | ✓ | - | - |
| utest | ✓ | ✓ | - | - |
| ZIO Test | ✓ | ✓ | - | - |
| specs2 | ✓ | ✓ | - | - |
| Weaver | ✓ | ✓ | - | - |
| JUnit 4 | ✓ | ✓ | ✓ | ✓ |
| **JUnit 5/Jupiter** | ✓ | ✓ | ✓ | ✓ |
| kotlin.test | - | - | - | ✓ |

**Total test cases**: ~48 framework/language combos × 4 scenarios = ~192 tests

---

## Project Structure

```
bleep-bsp-tests/src/scala/bleep/analysis/frameworks/
  TestFrameworkMatrixTest.scala      # Main orchestrator
  TestFrameworkConfig.scala          # Framework configuration
  TestScenario.scala                 # Scenario enum (Passing, Failing, Exception, Spectacular)
  sources/
    ScalaTestSources.scala           # ScalaTest templates (Scala 2.13/3)
    MUnitSources.scala               # MUnit templates
    JUnit5Sources.scala              # JUnit 5 templates (Scala/Java/Kotlin)
    JUnit4Sources.scala              # JUnit 4 templates
    UTestSources.scala               # utest templates
    ZioTestSources.scala             # ZIO Test templates
    Specs2Sources.scala              # specs2 templates
    WeaverSources.scala              # Weaver templates
    KotlinTestSources.scala          # kotlin.test templates
```

---

## Implementation Plan

### Phase 1: Core Infrastructure

**File**: `TestFrameworkConfig.scala`
```scala
case class TestFrameworkConfig(
  name: String,
  frameworkClass: String,
  supportedLanguages: Set[String],
  dependencies: List[String]
)

object TestFrameworks:
  val JUnit5 = TestFrameworkConfig(
    "JUnit5",
    "com.github.sbt.junit.JupiterFramework",
    Set("scala213", "scala3", "java", "kotlin"),
    List(
      "org.junit.jupiter:junit-jupiter-api:5.10.2",
      "org.junit.jupiter:junit-jupiter-engine:5.10.2",
      "net.aichler:jupiter-interface:0.11.1"
    )
  )
  // ... more frameworks
```

**File**: `TestScenario.scala`
```scala
enum TestScenario:
  case Passing      // Assert success
  case Failing      // Assert failure detected
  case Exception    // Assert exception captured
  case Spectacular  // Assert harness recovers (stack overflow, OOM)
```

### Phase 2: Source Templates (JUnit 5 Focus)

**JUnit 5 - Scala 3**:
```scala
object JUnit5Sources:
  def scala3(scenario: TestScenario): String = scenario match
    case Passing => """
      |package test.junit5
      |import org.junit.jupiter.api.{Test, Assertions}
      |class Junit5Test:
      |  @Test def passing(): Unit = Assertions.assertEquals(4, 2 + 2)
      |""".stripMargin
    case Failing => """
      |package test.junit5
      |import org.junit.jupiter.api.{Test, Assertions}
      |class Junit5Test:
      |  @Test def failing(): Unit = Assertions.assertEquals(5, 2 + 2, "should fail")
      |""".stripMargin
    case Exception => """
      |package test.junit5
      |import org.junit.jupiter.api.Test
      |class Junit5Test:
      |  @Test def throwing(): Unit = throw RuntimeException("boom!")
      |""".stripMargin
    case Spectacular => """
      |package test.junit5
      |import org.junit.jupiter.api.Test
      |class Junit5Test:
      |  @Test def stackOverflow(): Unit =
      |    def recurse(n: Int): Int = n + recurse(n + 1)
      |    recurse(0)
      |""".stripMargin
```

**JUnit 5 - Java**:
```scala
  def java(scenario: TestScenario): String = scenario match
    case Passing => """
      |package test.junit5;
      |import org.junit.jupiter.api.Test;
      |import static org.junit.jupiter.api.Assertions.*;
      |class Junit5Test {
      |  @Test void passing() { assertEquals(4, 2 + 2); }
      |}
      |""".stripMargin
    // ... other scenarios
```

### Phase 3: Test Orchestrator

**File**: `TestFrameworkMatrixTest.scala`
```scala
class TestFrameworkMatrixTest extends munit.FunSuite:
  import TestFrameworks.*
  import TestScenario.*

  val matrix: List[(TestFrameworkConfig, String, TestScenario)] =
    for
      framework <- List(JUnit5, JUnit4, ScalaTest, MUnit, UTest, ZioTest, Specs2, Weaver)
      language <- framework.supportedLanguages.toList
      scenario <- TestScenario.values.toList
    yield (framework, language, scenario)

  matrix.foreach { (framework, language, scenario) =>
    test(s"${framework.name} - $language - $scenario"):
      runScenario(framework, language, scenario)
  }

  def runScenario(framework: TestFrameworkConfig, language: String, scenario: TestScenario): Unit =
    val sources = generateSources(framework, language, scenario)
    val workspace = createTempWorkspace(sources)

    BspTestHarness.withProject(workspace) { client =>
      client.initialize()
      val targets = client.buildTargets()

      // Compile
      val compileResult = client.compile(targets.map(_.id))
      assertEquals(compileResult.statusCode.value, 1, "Compilation should succeed")

      // Run tests with timeout for spectacular failures
      val timeout = if scenario == Spectacular then 30.seconds else 2.minutes
      val testResult = client.testWithTimeout(targets.map(_.id), timeout)

      scenario match
        case Passing =>
          assertEquals(testResult.statusCode.value, 1, "Test should pass")
        case Failing =>
          assertEquals(testResult.statusCode.value, 2, "Test should fail")
          assert(testResult.hasFailureMessage, "Should have failure message")
        case Exception =>
          assertEquals(testResult.statusCode.value, 2, "Test should error")
          assert(testResult.hasStackTrace, "Should have stack trace")
        case Spectacular =>
          // May be error (2) or cancelled (3), but harness should not hang
          assert(testResult.statusCode.value >= 2, "Should fail or cancel")
    }
```

### Phase 4: Spectacular Failure Handling

The test harness must handle spectacular failures gracefully:

1. **Stack Overflow**: Forked JVM crashes → JvmPool detects dead process → Reports error
2. **OOM**: Forked JVM crashes → Same handling
3. **Deadlock**: Use timeout + cancellation → Force kill if needed
4. **Infinite Loop**: Timeout mechanism in TestRunner

Key: Set short timeouts (30s) for spectacular tests, verify harness recovers for next test.

---

## Dependencies to Add

Add to `bleep.yaml` for `bleep-bsp-tests`:

```yaml
bleep-bsp-tests:
  dependencies:
    # Test frameworks
    - org.junit.jupiter:junit-jupiter-api:5.10.2
    - org.junit.jupiter:junit-jupiter-engine:5.10.2
    - net.aichler:jupiter-interface:0.11.1
    - junit:junit:4.13.2
    - com.novocode:junit-interface:0.11
    - org.scalatest::scalatest:3.2.19
    - org.scalameta::munit:1.0.0
    - com.lihaoyi::utest:0.8.2
    - dev.zio::zio-test:2.0.19
    - dev.zio::zio-test-sbt:2.0.19
    - org.specs2::specs2-core:4.20.4
    - com.disneystreaming::weaver-cats:0.8.4
```

---

## Critical Files

| File | Purpose |
|------|---------|
| `bleep-bsp-tests/src/scala/bleep/analysis/BspTestHarness.scala` | Main test harness for BSP protocol |
| `bleep-bsp/src/scala/bleep/bsp/ClasspathTestDiscovery.scala` | Framework discovery |
| `bleep-test-runner/src/main/java/bleep/testing/runner/ForkedTestRunner.java` | Forked JVM test execution |
| `bleep-bsp/src/scala/bleep/bsp/TestRunner.scala` | Test orchestration with timeout/cancel |

---

## Verification

1. **Run matrix tests**: `bleep test bleep-bsp-tests --filter TestFrameworkMatrix`
2. **Verify JUnit 5 specifically**: Check jupiter-interface discovery works
3. **Verify spectacular recovery**: Run stack overflow test, then run a passing test immediately after
4. **Check event reporting**: Verify test events (started, passed, failed) are emitted correctly

---

## Commit Plan

1. `feat(tests): add TestFrameworkConfig and TestScenario types`
2. `feat(tests): add source templates for all frameworks`
3. `feat(tests): add JUnit 5 source templates (Scala/Java/Kotlin)`
4. `feat(tests): add TestFrameworkMatrixTest orchestrator`
5. `feat(tests): add spectacular failure handling with short timeouts`
6. `test(tests): verify all framework x language x scenario combinations`
