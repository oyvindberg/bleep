package bleep.analysis

import bleep.testing.FrameworkSelection
import bleep.testing.runner.TestProtocol.RunnerKind

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}
import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import scala.collection.mutable.ListBuffer

/** Comprehensive integration tests for ForkedTestRunner across all supported test frameworks.
  *
  * For each framework, three scenarios are tested:
  *   - Success: all tests pass
  *   - Failure: one test has an assertion failure
  *   - Throwing: one test throws an uncaught RuntimeException
  *
  * Frameworks tested: JUnit 4 (via vintage engine), JUnit 5 (Jupiter), ScalaTest, MUnit
  */
class ForkedTestRunnerFrameworkTest extends AnyFunSuite with Matchers with RunAndTestHelpers {

  def createTempDir(prefix: String): Path = Files.createTempDirectory(prefix)

  // ForkedTestRunner class location (bleep-test-runner compiled classes)
  lazy val testRunnerPath: Path = {
    val location = classOf[bleep.testing.runner.ForkedTestRunner].getProtectionDomain.getCodeSource.getLocation.toURI
    Path.of(location)
  }

  /** Structured result from running a suite via ForkedTestRunner protocol */
  case class SuiteRunResult(
      passed: Int,
      failed: Int,
      skipped: Int,
      outcome: String,
      testResults: List[(String, String)], // (testName, status)
      protocolLines: List[String],
      errors: List[String]
  )

  /** Launch ForkedTestRunner as a subprocess and run a single test suite via protocol */
  // The execution decision the server would have made, stated outright. These tests are the contract between the two: the fork is told which runner and, for
  // the sbt path, exactly which `Framework` class — it no longer infers either from the display name.
  private val junit4 = FrameworkSelection.JUnitPlatform("JUnit")
  private val junitJupiter = FrameworkSelection.JUnitPlatform("JUnit Jupiter")
  private val kotest = FrameworkSelection.JUnitPlatform("Kotest")
  private val scalaTest = FrameworkSelection.SbtTestInterface("ScalaTest", "org.scalatest.tools.Framework")
  private val munit = FrameworkSelection.SbtTestInterface("MUnit", "munit.Framework")
  private val utest = FrameworkSelection.SbtTestInterface("utest", "utest.runner.Framework")
  private val testng = FrameworkSelection.SbtTestInterface("TestNG", "mill.testng.TestNGFramework")

  def runSuiteViaProtocol(
      classpathEntries: Seq[Path],
      className: String,
      selection: FrameworkSelection
  ): SuiteRunResult = {
    val classpath = classpathEntries.map(_.toString).mkString(java.io.File.pathSeparator)
    val javaHome = System.getProperty("java.home")
    val javaBin = Path.of(javaHome, "bin", "java").toString

    val process = new ProcessBuilder(javaBin, "-cp", classpath, "bleep.testing.runner.ForkedTestRunner").start()
    val reader = new BufferedReader(new InputStreamReader(process.getInputStream))
    val writer = new PrintWriter(process.getOutputStream, true)
    val stderr = new BufferedReader(new InputStreamReader(process.getErrorStream))

    try {
      // Wait for Ready
      val readyLine = reader.readLine()
      assert(readyLine != null, "ForkedTestRunner did not send Ready")
      readyLine should include("\"type\":\"Ready\"")

      // Send RunSuite command
      val command = selection match {
        case FrameworkSelection.JUnitPlatform(displayName) =>
          bleep.testing.runner.TestProtocol.encodeRunSuite(className, displayName, RunnerKind.JUNIT_PLATFORM, null, java.util.List.of())
        case FrameworkSelection.SbtTestInterface(displayName, frameworkClass) =>
          bleep.testing.runner.TestProtocol.encodeRunSuite(className, displayName, RunnerKind.SBT_TEST_INTERFACE, frameworkClass, java.util.List.of())
        case other =>
          fail(s"$other is never sent over the protocol")
      }
      writer.println(command)

      // Collect all protocol output until SuiteDone
      val lines = collectUntilSuiteDone(reader)

      // Send shutdown
      writer.println(bleep.testing.runner.TestProtocol.encodeShutdown())

      // Parse results
      val testResults = ListBuffer[(String, String)]()
      val errors = ListBuffer[String]()
      var suitePassed = 0
      var suiteFailed = 0
      var suiteSkipped = 0
      var suiteOutcome = ""

      lines.foreach { line =>
        if (line.contains("\"type\":\"TestFinished\"")) {
          val status = extractJsonField(line, "status")
          val testName = extractJsonField(line, "test")
          testResults += ((testName, status))
        }
        if (line.contains("\"type\":\"SuiteDone\"")) {
          suitePassed = extractJsonIntField(line, "passed")
          suiteFailed = extractJsonIntField(line, "failed")
          suiteSkipped = extractJsonIntField(line, "skipped")
          suiteOutcome = extractJsonField(line, "outcome")
        }
        if (line.contains("\"type\":\"Error\"")) {
          errors += extractJsonField(line, "message")
        }
      }

      SuiteRunResult(
        passed = suitePassed,
        failed = suiteFailed,
        skipped = suiteSkipped,
        outcome = suiteOutcome,
        testResults = testResults.toList,
        protocolLines = lines,
        errors = errors.toList
      )
    } finally {
      process.destroyForcibly()
      reader.close()
      writer.close()
      stderr.close()
    }
  }

  /** Simple JSON field extractor (no dependency on JSON library) */
  private def extractJsonField(json: String, field: String): String = {
    val pattern = s""""$field":"([^"]*?)"""".r
    pattern.findFirstMatchIn(json).map(_.group(1)).getOrElse("")
  }

  private def extractJsonIntField(json: String, field: String): Int = {
    val pattern = s""""$field":(\\d+)""".r
    pattern.findFirstMatchIn(json).map(_.group(1).toInt).getOrElse(0)
  }

  def compileJava(sources: Seq[SourceFile], classpath: Seq[Path], outputDir: Path): Unit = {
    val input = CompilationInput(sources = sources, classpath = classpath, outputDir = outputDir, config = JavaConfig())
    val result = Compiler.forConfig(input.config).compile(input)
    result shouldBe a[CompilationSuccess]
  }

  def compileScala(sources: Seq[SourceFile], classpath: Seq[Path], outputDir: Path): Unit = {
    val input = CompilationInput(sources = sources, classpath = classpath, outputDir = outputDir, config = ScalaConfig(version = "3.7.4"))
    val result = Compiler.forConfig(input.config).compile(input)
    result shouldBe a[CompilationSuccess]
  }

  def compileKotlin(sources: Seq[SourceFile], classpath: Seq[Path], outputDir: Path): Unit = {
    val input = CompilationInput(sources = sources, classpath = classpath, outputDir = outputDir, config = KotlinConfig(version = "2.3.0"))
    val result = KotlinSourceCompiler.compile(input)
    result shouldBe a[CompilationSuccess]
  }

  // ============================================================================
  // JUnit 4 Test Sources
  // ============================================================================

  val junit4Success = SourceFile(
    Path.of("example/Junit4SuccessTest.java"),
    """package example;
      |
      |import org.junit.Test;
      |import static org.junit.Assert.*;
      |
      |public class Junit4SuccessTest {
      |    @Test
      |    public void additionWorks() {
      |        assertEquals(2, 1 + 1);
      |    }
      |
      |    @Test
      |    public void stringLength() {
      |        assertEquals(5, "hello".length());
      |    }
      |}
      |""".stripMargin
  )

  val junit4Failure = SourceFile(
    Path.of("example/Junit4FailureTest.java"),
    """package example;
      |
      |import org.junit.Test;
      |import static org.junit.Assert.*;
      |
      |public class Junit4FailureTest {
      |    @Test
      |    public void passingTest() {
      |        assertEquals(2, 1 + 1);
      |    }
      |
      |    @Test
      |    public void failingTest() {
      |        assertEquals("expected 3 but was 2", 3, 1 + 1);
      |    }
      |}
      |""".stripMargin
  )

  val junit4Throwing = SourceFile(
    Path.of("example/Junit4ThrowingTest.java"),
    """package example;
      |
      |import org.junit.Test;
      |import static org.junit.Assert.*;
      |
      |public class Junit4ThrowingTest {
      |    @Test
      |    public void passingTest() {
      |        assertEquals(2, 1 + 1);
      |    }
      |
      |    @Test
      |    public void throwingTest() {
      |        throw new RuntimeException("Unexpected error in test!");
      |    }
      |}
      |""".stripMargin
  )

  // ============================================================================
  // JUnit 5 Test Sources
  // ============================================================================

  val junit5Success = SourceFile(
    Path.of("example/Junit5SuccessTest.java"),
    """package example;
      |
      |import org.junit.jupiter.api.Test;
      |import static org.junit.jupiter.api.Assertions.*;
      |
      |public class Junit5SuccessTest {
      |    @Test
      |    public void additionWorks() {
      |        assertEquals(2, 1 + 1);
      |    }
      |
      |    @Test
      |    public void stringLength() {
      |        assertEquals(5, "hello".length());
      |    }
      |}
      |""".stripMargin
  )

  val junit5Failure = SourceFile(
    Path.of("example/Junit5FailureTest.java"),
    """package example;
      |
      |import org.junit.jupiter.api.Test;
      |import static org.junit.jupiter.api.Assertions.*;
      |
      |public class Junit5FailureTest {
      |    @Test
      |    public void passingTest() {
      |        assertEquals(2, 1 + 1);
      |    }
      |
      |    @Test
      |    public void failingTest() {
      |        assertEquals(3, 1 + 1, "expected 3 but was 2");
      |    }
      |}
      |""".stripMargin
  )

  val junit5Throwing = SourceFile(
    Path.of("example/Junit5ThrowingTest.java"),
    """package example;
      |
      |import org.junit.jupiter.api.Test;
      |import static org.junit.jupiter.api.Assertions.*;
      |
      |public class Junit5ThrowingTest {
      |    @Test
      |    public void passingTest() {
      |        assertEquals(2, 1 + 1);
      |    }
      |
      |    @Test
      |    public void throwingTest() {
      |        throw new RuntimeException("Unexpected error in test!");
      |    }
      |}
      |""".stripMargin
  )

  // ============================================================================
  // ScalaTest Test Sources
  // ============================================================================

  val scalaTestSuccess = SourceFile(
    Path.of("SuccessfulScalaTest.scala"),
    """package example
      |
      |import org.scalatest.funsuite.AnyFunSuite
      |
      |class SuccessfulScalaTest extends AnyFunSuite {
      |  test("addition works") {
      |    assert(1 + 1 == 2)
      |  }
      |  test("string length") {
      |    assert("hello".length == 5)
      |  }
      |}
      |""".stripMargin
  )

  val scalaTestFailure = SourceFile(
    Path.of("FailingScalaTest.scala"),
    """package example
      |
      |import org.scalatest.funsuite.AnyFunSuite
      |
      |class FailingScalaTest extends AnyFunSuite {
      |  test("passing test") {
      |    assert(1 + 1 == 2)
      |  }
      |  test("failing test") {
      |    assert(1 + 1 == 3)
      |  }
      |}
      |""".stripMargin
  )

  val scalaTestThrowing = SourceFile(
    Path.of("ThrowingScalaTest.scala"),
    """package example
      |
      |import org.scalatest.funsuite.AnyFunSuite
      |
      |class ThrowingScalaTest extends AnyFunSuite {
      |  test("passing test") {
      |    assert(1 + 1 == 2)
      |  }
      |  test("throwing test") {
      |    throw new RuntimeException("Unexpected error in test!")
      |  }
      |}
      |""".stripMargin
  )

  // ============================================================================
  // MUnit Test Sources
  // ============================================================================

  val munitSuccess = SourceFile(
    Path.of("SuccessfulMUnitTest.scala"),
    """package example
      |
      |class SuccessfulMUnitTest extends munit.FunSuite {
      |  test("addition works") {
      |    assertEquals(1 + 1, 2)
      |  }
      |  test("string length") {
      |    assertEquals("hello".length, 5)
      |  }
      |}
      |""".stripMargin
  )

  val munitFailure = SourceFile(
    Path.of("FailingMUnitTest.scala"),
    """package example
      |
      |class FailingMUnitTest extends munit.FunSuite {
      |  test("passing test") {
      |    assertEquals(1 + 1, 2)
      |  }
      |  test("failing test") {
      |    assertEquals(1 + 1, 3)
      |  }
      |}
      |""".stripMargin
  )

  val munitThrowing = SourceFile(
    Path.of("ThrowingMUnitTest.scala"),
    """package example
      |
      |class ThrowingMUnitTest extends munit.FunSuite {
      |  test("passing test") {
      |    assertEquals(1 + 1, 2)
      |  }
      |  test("throwing test") {
      |    throw new RuntimeException("Unexpected error in test!")
      |  }
      |}
      |""".stripMargin
  )

  // ============================================================================
  // JUnit 4 Tests (via vintage engine)
  // ============================================================================

  test("JUnit 4: all tests pass") {
    val outputDir = createTempDir("junit4-success-")
    try {
      compileJava(Seq(junit4Success), CompilerTestLibraries.junitLibrary, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++ CompilerTestLibraries.jupiterInterfaceLibrary ++ CompilerTestLibraries.junitLibrary
      val result = runSuiteViaProtocol(cp, "example.Junit4SuccessTest", junit4)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 2
      result.failed shouldBe 0
      result.testResults.size shouldBe 2
      result.testResults.foreach { case (_, status) => status shouldBe "passed" }
    } finally deleteRecursively(outputDir)
  }

  test("JUnit 4: assertion failure") {
    val outputDir = createTempDir("junit4-failure-")
    try {
      compileJava(Seq(junit4Failure), CompilerTestLibraries.junitLibrary, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++ CompilerTestLibraries.jupiterInterfaceLibrary ++ CompilerTestLibraries.junitLibrary
      val result = runSuiteViaProtocol(cp, "example.Junit4FailureTest", junit4)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 1
      result.failed shouldBe 1
      result.testResults.count(_._2 == "passed") shouldBe 1
      result.testResults.count(t => t._2 == "failed" || t._2 == "error") shouldBe 1
    } finally deleteRecursively(outputDir)
  }

  test("JUnit 4: uncaught exception") {
    val outputDir = createTempDir("junit4-throwing-")
    try {
      compileJava(Seq(junit4Throwing), CompilerTestLibraries.junitLibrary, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++ CompilerTestLibraries.jupiterInterfaceLibrary ++ CompilerTestLibraries.junitLibrary
      val result = runSuiteViaProtocol(cp, "example.Junit4ThrowingTest", junit4)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 1
      result.failed shouldBe 1
      result.testResults.count(_._2 == "passed") shouldBe 1
      result.testResults.count(t => t._2 == "failed" || t._2 == "error") shouldBe 1
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // JUnit 5 Tests (Jupiter)
  // ============================================================================

  test("JUnit 5: all tests pass") {
    val outputDir = createTempDir("junit5-success-")
    try {
      compileJava(Seq(junit5Success), CompilerTestLibraries.junit5Library, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++ CompilerTestLibraries.jupiterInterfaceLibrary ++ CompilerTestLibraries.junit5Library
      val result = runSuiteViaProtocol(cp, "example.Junit5SuccessTest", junitJupiter)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 2
      result.failed shouldBe 0
      result.testResults.size shouldBe 2
      result.testResults.foreach { case (_, status) => status shouldBe "passed" }
    } finally deleteRecursively(outputDir)
  }

  test("JUnit 5: assertion failure") {
    val outputDir = createTempDir("junit5-failure-")
    try {
      compileJava(Seq(junit5Failure), CompilerTestLibraries.junit5Library, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++ CompilerTestLibraries.jupiterInterfaceLibrary ++ CompilerTestLibraries.junit5Library
      val result = runSuiteViaProtocol(cp, "example.Junit5FailureTest", junitJupiter)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 1
      result.failed shouldBe 1
      result.testResults.count(_._2 == "passed") shouldBe 1
      result.testResults.count(t => t._2 == "failed" || t._2 == "error") shouldBe 1
    } finally deleteRecursively(outputDir)
  }

  test("JUnit 5: uncaught exception") {
    val outputDir = createTempDir("junit5-throwing-")
    try {
      compileJava(Seq(junit5Throwing), CompilerTestLibraries.junit5Library, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++ CompilerTestLibraries.jupiterInterfaceLibrary ++ CompilerTestLibraries.junit5Library
      val result = runSuiteViaProtocol(cp, "example.Junit5ThrowingTest", junitJupiter)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 1
      result.failed shouldBe 1
      result.testResults.count(_._2 == "passed") shouldBe 1
      result.testResults.count(t => t._2 == "failed" || t._2 == "error") shouldBe 1
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // ScalaTest Tests
  // ============================================================================

  test("ScalaTest: all tests pass") {
    val outputDir = createTempDir("scalatest-success-")
    try {
      val compileClasspath = CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.scalaTestLibrary
      compileScala(Seq(scalaTestSuccess), compileClasspath, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++
        CompilerTestLibraries.scalaTestLibrary ++ CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.testInterfaceLibrary
      val result = runSuiteViaProtocol(cp, "example.SuccessfulScalaTest", scalaTest)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 2
      result.failed shouldBe 0
      result.testResults.size shouldBe 2
      result.testResults.foreach { case (_, status) => status shouldBe "passed" }
    } finally deleteRecursively(outputDir)
  }

  test("ScalaTest: assertion failure") {
    val outputDir = createTempDir("scalatest-failure-")
    try {
      val compileClasspath = CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.scalaTestLibrary
      compileScala(Seq(scalaTestFailure), compileClasspath, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++
        CompilerTestLibraries.scalaTestLibrary ++ CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.testInterfaceLibrary
      val result = runSuiteViaProtocol(cp, "example.FailingScalaTest", scalaTest)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 1
      result.failed shouldBe 1
      result.testResults.count(_._2 == "passed") shouldBe 1
      result.testResults.count(t => t._2 == "failed" || t._2 == "error") shouldBe 1
    } finally deleteRecursively(outputDir)
  }

  test("ScalaTest: uncaught exception") {
    val outputDir = createTempDir("scalatest-throwing-")
    try {
      val compileClasspath = CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.scalaTestLibrary
      compileScala(Seq(scalaTestThrowing), compileClasspath, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++
        CompilerTestLibraries.scalaTestLibrary ++ CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.testInterfaceLibrary
      val result = runSuiteViaProtocol(cp, "example.ThrowingScalaTest", scalaTest)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 1
      result.failed shouldBe 1
      result.testResults.count(_._2 == "passed") shouldBe 1
      result.testResults.count(t => t._2 == "failed" || t._2 == "error") shouldBe 1
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // MUnit Tests
  // ============================================================================

  test("MUnit: all tests pass") {
    val outputDir = createTempDir("munit-success-")
    try {
      val compileClasspath = CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.munitLibrary
      compileScala(Seq(munitSuccess), compileClasspath, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++
        CompilerTestLibraries.munitLibrary ++ CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.testInterfaceLibrary
      val result = runSuiteViaProtocol(cp, "example.SuccessfulMUnitTest", munit)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 2
      result.failed shouldBe 0
      result.testResults.size shouldBe 2
      result.testResults.foreach { case (_, status) => status shouldBe "passed" }
    } finally deleteRecursively(outputDir)
  }

  test("MUnit: assertion failure") {
    val outputDir = createTempDir("munit-failure-")
    try {
      val compileClasspath = CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.munitLibrary
      compileScala(Seq(munitFailure), compileClasspath, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++
        CompilerTestLibraries.munitLibrary ++ CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.testInterfaceLibrary
      val result = runSuiteViaProtocol(cp, "example.FailingMUnitTest", munit)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 1
      result.failed shouldBe 1
      result.testResults.count(_._2 == "passed") shouldBe 1
      result.testResults.count(t => t._2 == "failed" || t._2 == "error") shouldBe 1
    } finally deleteRecursively(outputDir)
  }

  test("MUnit: uncaught exception") {
    val outputDir = createTempDir("munit-throwing-")
    try {
      val compileClasspath = CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.munitLibrary
      compileScala(Seq(munitThrowing), compileClasspath, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++
        CompilerTestLibraries.munitLibrary ++ CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.testInterfaceLibrary
      val result = runSuiteViaProtocol(cp, "example.ThrowingMUnitTest", munit)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 1
      result.failed shouldBe 1
      result.testResults.count(_._2 == "passed") shouldBe 1
      result.testResults.count(t => t._2 == "failed" || t._2 == "error") shouldBe 1
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // utest Test Sources
  // ============================================================================

  val utestSuccess = SourceFile(
    Path.of("SuccessfulUTest.scala"),
    """package example
      |
      |import utest._
      |
      |object SuccessfulUTest extends TestSuite {
      |  val tests = Tests {
      |    test("addition works") {
      |      assert(1 + 1 == 2)
      |    }
      |    test("string length") {
      |      assert("hello".length == 5)
      |    }
      |  }
      |}
      |""".stripMargin
  )

  val utestFailure = SourceFile(
    Path.of("FailingUTest.scala"),
    """package example
      |
      |import utest._
      |
      |object FailingUTest extends TestSuite {
      |  val tests = Tests {
      |    test("passing test") {
      |      assert(1 + 1 == 2)
      |    }
      |    test("failing test") {
      |      assert(1 + 1 == 3)
      |    }
      |  }
      |}
      |""".stripMargin
  )

  val utestThrowing = SourceFile(
    Path.of("ThrowingUTest.scala"),
    """package example
      |
      |import utest._
      |
      |object ThrowingUTest extends TestSuite {
      |  val tests = Tests {
      |    test("passing test") {
      |      assert(1 + 1 == 2)
      |    }
      |    test("throwing test") {
      |      throw new RuntimeException("Unexpected error in test!")
      |    }
      |  }
      |}
      |""".stripMargin
  )

  // ============================================================================
  // Kotest Test Sources (Kotlin)
  // ============================================================================

  val kotestSuccess = SourceFile(
    Path.of("example/SuccessfulKotest.kt"),
    """package example
      |
      |import io.kotest.core.spec.style.FunSpec
      |import io.kotest.matchers.shouldBe
      |
      |class SuccessfulKotest : FunSpec({
      |    test("addition works") {
      |        (1 + 1) shouldBe 2
      |    }
      |    test("string length") {
      |        "hello".length shouldBe 5
      |    }
      |})
      |""".stripMargin
  )

  val kotestFailure = SourceFile(
    Path.of("example/FailingKotest.kt"),
    """package example
      |
      |import io.kotest.core.spec.style.FunSpec
      |import io.kotest.matchers.shouldBe
      |
      |class FailingKotest : FunSpec({
      |    test("passing test") {
      |        (1 + 1) shouldBe 2
      |    }
      |    test("failing test") {
      |        (1 + 1) shouldBe 3
      |    }
      |})
      |""".stripMargin
  )

  val kotestThrowing = SourceFile(
    Path.of("example/ThrowingKotest.kt"),
    """package example
      |
      |import io.kotest.core.spec.style.FunSpec
      |import io.kotest.matchers.shouldBe
      |
      |class ThrowingKotest : FunSpec({
      |    test("passing test") {
      |        (1 + 1) shouldBe 2
      |    }
      |    test("throwing test") {
      |        throw RuntimeException("Unexpected error in test!")
      |    }
      |})
      |""".stripMargin
  )

  // ============================================================================
  // TestNG Test Sources (Java)
  // ============================================================================

  val testngSuccess = SourceFile(
    Path.of("example/TestNGSuccessTest.java"),
    """package example;
      |
      |import org.testng.annotations.Test;
      |import static org.testng.Assert.*;
      |
      |public class TestNGSuccessTest {
      |    @Test
      |    public void additionWorks() {
      |        assertEquals(1 + 1, 2);
      |    }
      |
      |    @Test
      |    public void stringLength() {
      |        assertEquals("hello".length(), 5);
      |    }
      |}
      |""".stripMargin
  )

  val testngFailure = SourceFile(
    Path.of("example/TestNGFailureTest.java"),
    """package example;
      |
      |import org.testng.annotations.Test;
      |import static org.testng.Assert.*;
      |
      |public class TestNGFailureTest {
      |    @Test
      |    public void passingTest() {
      |        assertEquals(1 + 1, 2);
      |    }
      |
      |    @Test
      |    public void failingTest() {
      |        assertEquals(1 + 1, 3, "expected 3 but was 2");
      |    }
      |}
      |""".stripMargin
  )

  val testngThrowing = SourceFile(
    Path.of("example/TestNGThrowingTest.java"),
    """package example;
      |
      |import org.testng.annotations.Test;
      |import static org.testng.Assert.*;
      |
      |public class TestNGThrowingTest {
      |    @Test
      |    public void passingTest() {
      |        assertEquals(1 + 1, 2);
      |    }
      |
      |    @Test
      |    public void throwingTest() {
      |        throw new RuntimeException("Unexpected error in test!");
      |    }
      |}
      |""".stripMargin
  )

  // ============================================================================
  // utest Tests
  // ============================================================================

  test("utest: all tests pass") {
    val outputDir = createTempDir("utest-success-")
    try {
      val compileClasspath = CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.utestLibrary
      compileScala(Seq(utestSuccess), compileClasspath, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++
        CompilerTestLibraries.utestLibrary ++ CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.testInterfaceLibrary
      val result = runSuiteViaProtocol(cp, "example.SuccessfulUTest", utest)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 2
      result.failed shouldBe 0
      result.testResults.size shouldBe 2
      result.testResults.foreach { case (_, status) => status shouldBe "passed" }
    } finally deleteRecursively(outputDir)
  }

  test("utest: assertion failure") {
    val outputDir = createTempDir("utest-failure-")
    try {
      val compileClasspath = CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.utestLibrary
      compileScala(Seq(utestFailure), compileClasspath, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++
        CompilerTestLibraries.utestLibrary ++ CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.testInterfaceLibrary
      val result = runSuiteViaProtocol(cp, "example.FailingUTest", utest)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 1
      result.failed shouldBe 1
      result.testResults.count(_._2 == "passed") shouldBe 1
      result.testResults.count(t => t._2 == "failed" || t._2 == "error") shouldBe 1
    } finally deleteRecursively(outputDir)
  }

  test("utest: uncaught exception") {
    val outputDir = createTempDir("utest-throwing-")
    try {
      val compileClasspath = CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.utestLibrary
      compileScala(Seq(utestThrowing), compileClasspath, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++
        CompilerTestLibraries.utestLibrary ++ CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.testInterfaceLibrary
      val result = runSuiteViaProtocol(cp, "example.ThrowingUTest", utest)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 1
      result.failed shouldBe 1
      result.testResults.count(_._2 == "passed") shouldBe 1
      result.testResults.count(t => t._2 == "failed" || t._2 == "error") shouldBe 1
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // Kotest Tests (Kotlin, via JUnit Platform)
  // ============================================================================

  test("Kotest: all tests pass") {
    val outputDir = createTempDir("kotest-success-")
    try {
      val compileClasspath = CompilerTestLibraries.kotlinLibrary ++ CompilerTestLibraries.kotestLibrary
      compileKotlin(Seq(kotestSuccess), compileClasspath, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++
        CompilerTestLibraries.jupiterInterfaceLibrary ++ CompilerTestLibraries.kotestLibrary ++ CompilerTestLibraries.kotlinLibrary
      val result = runSuiteViaProtocol(cp, "example.SuccessfulKotest", kotest)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 2
      result.failed shouldBe 0
      result.testResults.size shouldBe 2
      result.testResults.foreach { case (_, status) => status shouldBe "passed" }
    } finally deleteRecursively(outputDir)
  }

  test("Kotest: assertion failure") {
    val outputDir = createTempDir("kotest-failure-")
    try {
      val compileClasspath = CompilerTestLibraries.kotlinLibrary ++ CompilerTestLibraries.kotestLibrary
      compileKotlin(Seq(kotestFailure), compileClasspath, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++
        CompilerTestLibraries.jupiterInterfaceLibrary ++ CompilerTestLibraries.kotestLibrary ++ CompilerTestLibraries.kotlinLibrary
      val result = runSuiteViaProtocol(cp, "example.FailingKotest", kotest)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 1
      result.failed shouldBe 1
      result.testResults.count(_._2 == "passed") shouldBe 1
      result.testResults.count(t => t._2 == "failed" || t._2 == "error") shouldBe 1
    } finally deleteRecursively(outputDir)
  }

  test("Kotest: uncaught exception") {
    val outputDir = createTempDir("kotest-throwing-")
    try {
      val compileClasspath = CompilerTestLibraries.kotlinLibrary ++ CompilerTestLibraries.kotestLibrary
      compileKotlin(Seq(kotestThrowing), compileClasspath, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++
        CompilerTestLibraries.jupiterInterfaceLibrary ++ CompilerTestLibraries.kotestLibrary ++ CompilerTestLibraries.kotlinLibrary
      val result = runSuiteViaProtocol(cp, "example.ThrowingKotest", kotest)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 1
      result.failed shouldBe 1
      result.testResults.count(_._2 == "passed") shouldBe 1
      result.testResults.count(t => t._2 == "failed" || t._2 == "error") shouldBe 1
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // TestNG Tests (Java, via Mill bridge)
  // ============================================================================

  test("TestNG: all tests pass") {
    val outputDir = createTempDir("testng-success-")
    try {
      compileJava(Seq(testngSuccess), CompilerTestLibraries.testngLibrary, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++
        CompilerTestLibraries.testngBridgeLibrary ++ CompilerTestLibraries.testngLibrary ++ CompilerTestLibraries.testInterfaceLibrary
      val result = runSuiteViaProtocol(cp, "example.TestNGSuccessTest", testng)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 2
      result.failed shouldBe 0
      result.testResults.size shouldBe 2
      result.testResults.foreach { case (_, status) => status shouldBe "passed" }
    } finally deleteRecursively(outputDir)
  }

  test("TestNG: assertion failure") {
    val outputDir = createTempDir("testng-failure-")
    try {
      compileJava(Seq(testngFailure), CompilerTestLibraries.testngLibrary, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++
        CompilerTestLibraries.testngBridgeLibrary ++ CompilerTestLibraries.testngLibrary ++ CompilerTestLibraries.testInterfaceLibrary
      val result = runSuiteViaProtocol(cp, "example.TestNGFailureTest", testng)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 1
      result.failed shouldBe 1
      result.testResults.count(_._2 == "passed") shouldBe 1
      result.testResults.count(t => t._2 == "failed" || t._2 == "error") shouldBe 1
    } finally deleteRecursively(outputDir)
  }

  test("TestNG: uncaught exception") {
    val outputDir = createTempDir("testng-throwing-")
    try {
      compileJava(Seq(testngThrowing), CompilerTestLibraries.testngLibrary, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++
        CompilerTestLibraries.testngBridgeLibrary ++ CompilerTestLibraries.testngLibrary ++ CompilerTestLibraries.testInterfaceLibrary
      val result = runSuiteViaProtocol(cp, "example.TestNGThrowingTest", testng)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 1
      result.failed shouldBe 1
      result.testResults.count(_._2 == "passed") shouldBe 1
      result.testResults.count(t => t._2 == "failed" || t._2 == "error") shouldBe 1
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // Container-level lifecycle failures (JUnit Platform)
  //
  // @AfterClass/@AfterAll and friends are reported by the platform against the *container*, not
  // against any test. The listener used to drop every non-test identifier, so a class whose
  // teardown asserted was reported green with exit 0.
  // ============================================================================

  val junit4AfterClassFailure = SourceFile(
    Path.of("example/Junit4AfterClassFailureTest.java"),
    """package example;
      |
      |import org.junit.AfterClass;
      |import org.junit.Test;
      |import static org.junit.Assert.*;
      |
      |public class Junit4AfterClassFailureTest {
      |    @Test
      |    public void passingTest() {
      |        assertEquals(2, 1 + 1);
      |    }
      |
      |    @AfterClass
      |    public static void afterAll() {
      |        throw new AssertionError("deliberate @AfterClass failure");
      |    }
      |}
      |""".stripMargin
  )

  val junit4ParameterizedAfterClassFailure = SourceFile(
    Path.of("example/Junit4ParameterizedAfterClassTest.java"),
    """package example;
      |
      |import java.util.Arrays;
      |import java.util.List;
      |import org.junit.AfterClass;
      |import org.junit.Test;
      |import org.junit.runner.RunWith;
      |import org.junit.runners.Parameterized;
      |
      |@RunWith(Parameterized.class)
      |public class Junit4ParameterizedAfterClassTest {
      |    @Parameterized.Parameters(name = "{0}")
      |    public static List<Object[]> params() {
      |        return Arrays.asList(new Object[] {"a"}, new Object[] {"b"});
      |    }
      |
      |    @Parameterized.Parameter(0)
      |    public String name;
      |
      |    @Test
      |    public void passes() {}
      |
      |    @AfterClass
      |    public static void afterAll() {
      |        throw new AssertionError("deliberate @AfterClass failure");
      |    }
      |}
      |""".stripMargin
  )

  val junit5AfterAllFailure = SourceFile(
    Path.of("example/Junit5AfterAllFailureTest.java"),
    """package example;
      |
      |import org.junit.jupiter.api.AfterAll;
      |import org.junit.jupiter.api.Test;
      |import static org.junit.jupiter.api.Assertions.*;
      |
      |public class Junit5AfterAllFailureTest {
      |    @Test
      |    public void passingTest() {
      |        assertEquals(2, 1 + 1);
      |    }
      |
      |    @AfterAll
      |    public static void afterAll() {
      |        fail("deliberate @AfterAll failure");
      |    }
      |}
      |""".stripMargin
  )

  val junit5BeforeAllFailure = SourceFile(
    Path.of("example/Junit5BeforeAllFailureTest.java"),
    """package example;
      |
      |import org.junit.jupiter.api.BeforeAll;
      |import org.junit.jupiter.api.Test;
      |import static org.junit.jupiter.api.Assertions.*;
      |
      |public class Junit5BeforeAllFailureTest {
      |    @BeforeAll
      |    public static void beforeAll() {
      |        fail("deliberate @BeforeAll failure");
      |    }
      |
      |    @Test
      |    public void neverRuns() {
      |        assertEquals(2, 1 + 1);
      |    }
      |}
      |""".stripMargin
  )

  val junit5DisabledClass = SourceFile(
    Path.of("example/Junit5DisabledClassTest.java"),
    """package example;
      |
      |import org.junit.jupiter.api.Disabled;
      |import org.junit.jupiter.api.Test;
      |import static org.junit.jupiter.api.Assertions.*;
      |
      |@Disabled("not today")
      |public class Junit5DisabledClassTest {
      |    @Test
      |    public void neverRuns() {
      |        assertEquals(2, 1 + 1);
      |    }
      |}
      |""".stripMargin
  )

  test("JUnit 4: @AfterClass failure is reported against the class") {
    val outputDir = createTempDir("junit4-afterclass-")
    try {
      compileJava(Seq(junit4AfterClassFailure), CompilerTestLibraries.junitLibrary, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++ CompilerTestLibraries.jupiterInterfaceLibrary ++ CompilerTestLibraries.junitLibrary
      val result = runSuiteViaProtocol(cp, "example.Junit4AfterClassFailureTest", junit4)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 1
      result.failed shouldBe 1
      result.outcome shouldBe "executed"
      val failures = result.testResults.filter(_._2 == "failed")
      failures.size shouldBe 1
      failures.head._1 should include("class-level")
      result.protocolLines.mkString("\n") should include("deliberate @AfterClass failure")
    } finally deleteRecursively(outputDir)
  }

  test("JUnit 4: @AfterClass failure in a @Parameterized class is reported against the class") {
    val outputDir = createTempDir("junit4-param-afterclass-")
    try {
      compileJava(Seq(junit4ParameterizedAfterClassFailure), CompilerTestLibraries.junitLibrary, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++ CompilerTestLibraries.jupiterInterfaceLibrary ++ CompilerTestLibraries.junitLibrary
      val result = runSuiteViaProtocol(cp, "example.Junit4ParameterizedAfterClassTest", junit4)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 2
      result.failed shouldBe 1
      result.outcome shouldBe "executed"
      result.testResults.count(_._2 == "failed") shouldBe 1
    } finally deleteRecursively(outputDir)
  }

  test("JUnit 5: @AfterAll failure is reported against the class") {
    val outputDir = createTempDir("junit5-afterall-")
    try {
      compileJava(Seq(junit5AfterAllFailure), CompilerTestLibraries.junit5Library, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++ CompilerTestLibraries.jupiterInterfaceLibrary ++ CompilerTestLibraries.junit5Library
      val result = runSuiteViaProtocol(cp, "example.Junit5AfterAllFailureTest", junitJupiter)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 1
      result.failed shouldBe 1
      result.outcome shouldBe "executed"
      val failures = result.testResults.filter(_._2 == "failed")
      failures.size shouldBe 1
      failures.head._1 should include("class-level")
      result.protocolLines.mkString("\n") should include("deliberate @AfterAll failure")
    } finally deleteRecursively(outputDir)
  }

  test("JUnit 5: @BeforeAll failure is reported as a failure, not an empty suite") {
    val outputDir = createTempDir("junit5-beforeall-")
    try {
      compileJava(Seq(junit5BeforeAllFailure), CompilerTestLibraries.junit5Library, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++ CompilerTestLibraries.jupiterInterfaceLibrary ++ CompilerTestLibraries.junit5Library
      val result = runSuiteViaProtocol(cp, "example.Junit5BeforeAllFailureTest", junitJupiter)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 0
      result.failed shouldBe 1
      result.outcome shouldBe "executed"
      result.protocolLines.mkString("\n") should include("deliberate @BeforeAll failure")
    } finally deleteRecursively(outputDir)
  }

  test("JUnit 5: a @Disabled class is skipped, not an empty suite") {
    val outputDir = createTempDir("junit5-disabled-")
    try {
      compileJava(Seq(junit5DisabledClass), CompilerTestLibraries.junit5Library, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++ CompilerTestLibraries.jupiterInterfaceLibrary ++ CompilerTestLibraries.junit5Library
      val result = runSuiteViaProtocol(cp, "example.Junit5DisabledClassTest", junitJupiter)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 0
      result.failed shouldBe 0
      result.skipped shouldBe 1
      result.outcome shouldBe "executed"
    } finally deleteRecursively(outputDir)
  }

  val junit4EmptyParams = SourceFile(
    Path.of("example/Junit4EmptyParamsTest.java"),
    """package example;
      |
      |import java.util.Collections;
      |import java.util.List;
      |import org.junit.Test;
      |import org.junit.runner.RunWith;
      |import org.junit.runners.Parameterized;
      |
      |@RunWith(Parameterized.class)
      |public class Junit4EmptyParamsTest {
      |    @Parameterized.Parameters(name = "{0}")
      |    public static List<Object[]> params() {
      |        return Collections.emptyList();
      |    }
      |
      |    @Parameterized.Parameter(0)
      |    public String name;
      |
      |    @Test
      |    public void passes() {}
      |}
      |""".stripMargin
  )

  test("JUnit 4: a class with an empty @Parameters list is empty, not one passed test") {
    val outputDir = createTempDir("junit4-emptyparams-")
    try {
      compileJava(Seq(junit4EmptyParams), CompilerTestLibraries.junitLibrary, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++ CompilerTestLibraries.jupiterInterfaceLibrary ++ CompilerTestLibraries.junitLibrary
      val result = runSuiteViaProtocol(cp, "example.Junit4EmptyParamsTest", junit4)

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 0
      result.failed shouldBe 0
      result.outcome shouldBe "empty"
      result.testResults shouldBe empty
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // Class-level teardown failures on the sbt-testing path
  //
  // Same question as the JUnit Platform bridge above, asked of every framework that goes through
  // sbt's test-interface instead: when the failure belongs to the class rather than to any one
  // test, does it reach us at all?
  // ============================================================================

  val scalaTestAfterAllFailure = SourceFile(
    Path.of("AfterAllFailingScalaTest.scala"),
    """package example
      |
      |import org.scalatest.BeforeAndAfterAll
      |import org.scalatest.funsuite.AnyFunSuite
      |
      |class AfterAllFailingScalaTest extends AnyFunSuite with BeforeAndAfterAll {
      |  test("passing test") {
      |    assert(1 + 1 == 2)
      |  }
      |  override def afterAll(): Unit =
      |    throw new AssertionError("deliberate afterAll failure")
      |}
      |""".stripMargin
  )

  val munitAfterAllFailure = SourceFile(
    Path.of("AfterAllFailingMUnitTest.scala"),
    """package example
      |
      |class AfterAllFailingMUnitTest extends munit.FunSuite {
      |  test("passing test") {
      |    assertEquals(1 + 1, 2)
      |  }
      |  override def afterAll(): Unit =
      |    throw new AssertionError("deliberate afterAll failure")
      |}
      |""".stripMargin
  )

  val utestAfterAllFailure = SourceFile(
    Path.of("AfterAllFailingUTest.scala"),
    """package example
      |
      |import utest._
      |
      |object AfterAllFailingUTest extends TestSuite {
      |  val tests = Tests {
      |    test("passing test") {
      |      assert(1 + 1 == 2)
      |    }
      |  }
      |  // java.lang.AssertionError spelled out: `import utest._` shadows it with utest.AssertionError
      |  override def utestAfterAll(): Unit =
      |    throw new java.lang.AssertionError("deliberate utestAfterAll failure")
      |}
      |""".stripMargin
  )

  val kotestAfterSpecFailure = SourceFile(
    Path.of("example/AfterSpecFailingKotest.kt"),
    """package example
      |
      |import io.kotest.core.spec.style.FunSpec
      |import io.kotest.matchers.shouldBe
      |
      |class AfterSpecFailingKotest : FunSpec({
      |    test("addition works") {
      |        (1 + 1) shouldBe 2
      |    }
      |    afterSpec {
      |        throw AssertionError("deliberate afterSpec failure")
      |    }
      |})
      |""".stripMargin
  )

  val testngAfterClassFailure = SourceFile(
    Path.of("example/TestNGAfterClassFailureTest.java"),
    """package example;
      |
      |import org.testng.annotations.AfterClass;
      |import org.testng.annotations.Test;
      |import static org.testng.Assert.*;
      |
      |public class TestNGAfterClassFailureTest {
      |    @Test
      |    public void passingTest() {
      |        assertEquals(1 + 1, 2);
      |    }
      |
      |    @AfterClass
      |    public void afterAll() {
      |        throw new AssertionError("deliberate @AfterClass failure");
      |    }
      |}
      |""".stripMargin
  )

  test("ScalaTest: afterAll failure is not swallowed") {
    val outputDir = createTempDir("scalatest-afterall-")
    try {
      val compileClasspath = CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.scalaTestLibrary
      compileScala(Seq(scalaTestAfterAllFailure), compileClasspath, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++
        CompilerTestLibraries.scalaTestLibrary ++ CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.testInterfaceLibrary
      val result = runSuiteViaProtocol(cp, "example.AfterAllFailingScalaTest", scalaTest)

      info(s"outcome=${result.outcome} passed=${result.passed} failed=${result.failed} results=${result.testResults}")
      withClue(result.protocolLines.mkString("\n")) {
        (result.failed > 0 || result.outcome == "errored") shouldBe true
      }
    } finally deleteRecursively(outputDir)
  }

  test("MUnit: afterAll failure is not swallowed") {
    val outputDir = createTempDir("munit-afterall-")
    try {
      val compileClasspath = CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.munitLibrary
      compileScala(Seq(munitAfterAllFailure), compileClasspath, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++
        CompilerTestLibraries.munitLibrary ++ CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.testInterfaceLibrary
      val result = runSuiteViaProtocol(cp, "example.AfterAllFailingMUnitTest", munit)

      info(s"outcome=${result.outcome} passed=${result.passed} failed=${result.failed} results=${result.testResults}")
      withClue(result.protocolLines.mkString("\n")) {
        (result.failed > 0 || result.outcome == "errored") shouldBe true
      }
    } finally deleteRecursively(outputDir)
  }

  test("utest: utestAfterAll failure is not swallowed") {
    val outputDir = createTempDir("utest-afterall-")
    try {
      val compileClasspath = CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.utestLibrary
      compileScala(Seq(utestAfterAllFailure), compileClasspath, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++
        CompilerTestLibraries.utestLibrary ++ CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.testInterfaceLibrary
      val result = runSuiteViaProtocol(cp, "example.AfterAllFailingUTest", utest)

      info(s"outcome=${result.outcome} passed=${result.passed} failed=${result.failed} results=${result.testResults}")
      withClue(result.protocolLines.mkString("\n")) {
        (result.failed > 0 || result.outcome == "errored") shouldBe true
      }
    } finally deleteRecursively(outputDir)
  }

  test("Kotest: afterSpec failure is not swallowed") {
    val outputDir = createTempDir("kotest-afterspec-")
    try {
      val compileClasspath = CompilerTestLibraries.kotlinLibrary ++ CompilerTestLibraries.kotestLibrary
      compileKotlin(Seq(kotestAfterSpecFailure), compileClasspath, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++
        CompilerTestLibraries.jupiterInterfaceLibrary ++ CompilerTestLibraries.kotestLibrary ++ CompilerTestLibraries.kotlinLibrary
      val result = runSuiteViaProtocol(cp, "example.AfterSpecFailingKotest", kotest)

      info(s"outcome=${result.outcome} passed=${result.passed} failed=${result.failed} results=${result.testResults}")
      withClue(result.protocolLines.mkString("\n")) {
        (result.failed > 0 || result.outcome == "errored") shouldBe true
      }
    } finally deleteRecursively(outputDir)
  }

  // TestNG is the one framework where a class-level teardown failure never reaches us, and the
  // limitation is not ours to fix here: the sbt-testing bridge bleep discovers on the user's
  // classpath, `mill.testng.TestNGListener`, implements `org.testng.ITestListener` and nothing
  // else. TestNG routes @BeforeClass/@AfterClass failures to `IConfigurationListener` instead, so
  // the bridge emits no event for them — TestNG prints "Configuration Failures: 1" and we are told
  // only about the passing test. This test pins that behaviour rather than pretending it is fine;
  // when a bridge that implements IConfigurationListener shows up, it fails and we come back.
  //
  // A @BeforeClass failure is less severe: TestNG skips the dependent tests, and skips do go
  // through ITestListener, so the suite at least does not look green.
  test("TestNG: @AfterClass failure is invisible — the mill bridge reports no configuration events") {
    val outputDir = createTempDir("testng-afterclass-")
    try {
      compileJava(Seq(testngAfterClassFailure), CompilerTestLibraries.testngLibrary, outputDir)
      val cp = Seq(outputDir, testRunnerPath) ++
        CompilerTestLibraries.testngBridgeLibrary ++ CompilerTestLibraries.testngLibrary ++ CompilerTestLibraries.testInterfaceLibrary
      val result = runSuiteViaProtocol(cp, "example.TestNGAfterClassFailureTest", testng)

      info(s"outcome=${result.outcome} passed=${result.passed} failed=${result.failed} results=${result.testResults}")
      withClue(result.protocolLines.mkString("\n")) {
        result.passed shouldBe 1
        result.failed shouldBe 0
        // TestNG itself knows; it just has no way to tell the bridge.
        result.protocolLines.mkString("\n") should include("Configuration Failures: 1")
      }
    } finally deleteRecursively(outputDir)
  }
}
