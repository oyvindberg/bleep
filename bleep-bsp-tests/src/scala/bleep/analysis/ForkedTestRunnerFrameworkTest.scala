package bleep.analysis

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

  def deleteRecursively(path: Path): Unit =
    if Files.exists(path) then {
      if Files.isDirectory(path) then {
        import scala.jdk.StreamConverters.*
        Files.list(path).toScala(List).foreach(deleteRecursively)
      }
      Files.delete(path)
    }

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
      testResults: List[(String, String)], // (testName, status)
      protocolLines: List[String],
      errors: List[String]
  )

  /** Launch ForkedTestRunner as a subprocess and run a single test suite via protocol */
  def runSuiteViaProtocol(
      classpathEntries: Seq[Path],
      className: String,
      frameworkName: String
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
      val command = bleep.testing.runner.TestProtocol.encodeRunSuite(className, frameworkName, java.util.List.of())
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
        }
        if (line.contains("\"type\":\"Error\"")) {
          errors += extractJsonField(line, "message")
        }
      }

      SuiteRunResult(
        passed = suitePassed,
        failed = suiteFailed,
        skipped = suiteSkipped,
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
      val result = runSuiteViaProtocol(cp, "example.Junit4SuccessTest", "JUnit")

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
      val result = runSuiteViaProtocol(cp, "example.Junit4FailureTest", "JUnit")

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
      val result = runSuiteViaProtocol(cp, "example.Junit4ThrowingTest", "JUnit")

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
      val result = runSuiteViaProtocol(cp, "example.Junit5SuccessTest", "JUnit Jupiter")

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
      val result = runSuiteViaProtocol(cp, "example.Junit5FailureTest", "JUnit Jupiter")

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
      val result = runSuiteViaProtocol(cp, "example.Junit5ThrowingTest", "JUnit Jupiter")

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
      val result = runSuiteViaProtocol(cp, "example.SuccessfulScalaTest", "ScalaTest")

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
      val result = runSuiteViaProtocol(cp, "example.FailingScalaTest", "ScalaTest")

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
      val result = runSuiteViaProtocol(cp, "example.ThrowingScalaTest", "ScalaTest")

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
      val result = runSuiteViaProtocol(cp, "example.SuccessfulMUnitTest", "MUnit")

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
      val result = runSuiteViaProtocol(cp, "example.FailingMUnitTest", "MUnit")

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
      val result = runSuiteViaProtocol(cp, "example.ThrowingMUnitTest", "MUnit")

      info(s"Test results: ${result.testResults}")
      result.passed shouldBe 1
      result.failed shouldBe 1
      result.testResults.count(_._2 == "passed") shouldBe 1
      result.testResults.count(t => t._2 == "failed" || t._2 == "error") shouldBe 1
    } finally deleteRecursively(outputDir)
  }
}
