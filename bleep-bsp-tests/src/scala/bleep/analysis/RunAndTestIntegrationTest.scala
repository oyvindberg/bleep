package bleep.analysis

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}
import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import scala.collection.mutable.ListBuffer

/** Integration tests for running and testing compiled code.
  *
  * These tests verify that:
  *   1. Main classes can be compiled and executed
  *   2. Test classes can be compiled and run with test frameworks
  *   3. All three languages (Scala, Kotlin, Java) work correctly
  */
class RunAndTestIntegrationTest extends AnyFunSuite with Matchers with RunAndTestHelpers {

  def createTempDir(prefix: String): Path =
    Files.createTempDirectory(prefix)

  def deleteRecursively(path: Path): Unit =
    if Files.exists(path) then {
      if Files.isDirectory(path) then {
        import scala.jdk.StreamConverters.*
        Files.list(path).toScala(List).foreach(deleteRecursively)
      }
      Files.delete(path)
    }

  // ============================================================================
  // Scala Main Class Tests
  // ============================================================================

  val scalaMainSource = SourceFile(
    Path.of("Main.scala"),
    """package example
      |
      |object Main:
      |  def main(args: Array[String]): Unit =
      |    val message = if args.isEmpty then "Hello, World!" else args.mkString(" ")
      |    println(message)
      |    println(s"Args count: ${args.length}")
      |""".stripMargin
  )

  test("Scala: compile and verify main class exists") {
    val outputDir = createTempDir("scala-main-")
    try {
      val input = CompilationInput(
        sources = Seq(scalaMainSource),
        classpath = CompilerTestLibraries.scalaLibrary,
        outputDir = outputDir,
        config = ScalaConfig(version = "3.7.4")
      )

      val result = Compiler.forConfig(input.config).compile(input)
      result shouldBe a[CompilationSuccess]

      val success = result.asInstanceOf[CompilationSuccess]
      success.compiledClasses should not be empty

      // Check that Main class exists
      val mainClass = success.compiledClasses.find(_.toString.contains("Main"))
      mainClass shouldBe defined
      info(s"Compiled ${success.compiledClasses.size} class files including Main")
    } finally deleteRecursively(outputDir)
  }

  test("Scala: run main class") {
    val outputDir = createTempDir("scala-run-")
    try {
      val input = CompilationInput(
        sources = Seq(scalaMainSource),
        classpath = CompilerTestLibraries.scalaLibrary,
        outputDir = outputDir,
        config = ScalaConfig(version = "3.7.4")
      )

      val result = Compiler.forConfig(input.config).compile(input)
      result shouldBe a[CompilationSuccess]

      // Run the main class
      val classpath = (outputDir :: CompilerTestLibraries.scalaLibrary.toList)
        .map(_.toString)
        .mkString(java.io.File.pathSeparator)

      val javaHome = System.getProperty("java.home")
      val javaBin = Path.of(javaHome, "bin", "java").toString

      val process = new ProcessBuilder(javaBin, "-cp", classpath, "example.Main", "test", "args")
        .redirectErrorStream(true)
        .start()

      val output = new String(process.getInputStream.readAllBytes())
      val exitCode = process.waitFor()

      exitCode shouldBe 0
      output should include("test args")
      output should include("Args count: 2")
      info(s"Main class ran successfully with output: ${output.trim}")
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // Kotlin Main Class Tests
  // ============================================================================

  val kotlinMainSource = SourceFile(
    Path.of("Main.kt"),
    """package example
      |
      |fun main(args: Array<String>) {
      |    val message = if (args.isEmpty()) "Hello, World!" else args.joinToString(" ")
      |    println(message)
      |    println("Args count: ${args.size}")
      |}
      |""".stripMargin
  )

  test("Kotlin: compile and verify main class exists") {
    val outputDir = createTempDir("kotlin-main-")
    try {
      val input = CompilationInput(
        sources = Seq(kotlinMainSource),
        classpath = CompilerTestLibraries.kotlinLibrary,
        outputDir = outputDir,
        config = KotlinConfig(version = "2.3.0")
      )

      val result = KotlinSourceCompiler.compile(input)
      result shouldBe a[CompilationSuccess]

      val success = result.asInstanceOf[CompilationSuccess]
      success.compiledClasses should not be empty

      // Kotlin generates MainKt class for top-level functions
      val mainClass = success.compiledClasses.find(_.toString.contains("MainKt"))
      mainClass shouldBe defined
      info(s"Compiled ${success.compiledClasses.size} class files including MainKt")
    } finally deleteRecursively(outputDir)
  }

  test("Kotlin: run main class") {
    val outputDir = createTempDir("kotlin-run-")
    try {
      val input = CompilationInput(
        sources = Seq(kotlinMainSource),
        classpath = CompilerTestLibraries.kotlinLibrary,
        outputDir = outputDir,
        config = KotlinConfig(version = "2.3.0")
      )

      val result = KotlinSourceCompiler.compile(input)
      result shouldBe a[CompilationSuccess]

      // Run the main class
      val classpath = (outputDir :: CompilerTestLibraries.kotlinLibrary.toList)
        .map(_.toString)
        .mkString(java.io.File.pathSeparator)

      val javaHome = System.getProperty("java.home")
      val javaBin = Path.of(javaHome, "bin", "java").toString

      val process = new ProcessBuilder(javaBin, "-cp", classpath, "example.MainKt", "kotlin", "test")
        .redirectErrorStream(true)
        .start()

      val output = new String(process.getInputStream.readAllBytes())
      val exitCode = process.waitFor()

      exitCode shouldBe 0
      output should include("kotlin test")
      output should include("Args count: 2")
      info(s"Main class ran successfully with output: ${output.trim}")
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // Java Main Class Tests
  // ============================================================================

  val javaMainSource = SourceFile(
    Path.of("example/Main.java"),
    """package example;
      |
      |public class Main {
      |    public static void main(String[] args) {
      |        String message = args.length == 0 ? "Hello, World!" : String.join(" ", args);
      |        System.out.println(message);
      |        System.out.println("Args count: " + args.length);
      |    }
      |}
      |""".stripMargin
  )

  test("Java: compile and verify main class exists") {
    val outputDir = createTempDir("java-main-")
    try {
      val input = CompilationInput(
        sources = Seq(javaMainSource),
        classpath = Seq.empty,
        outputDir = outputDir,
        config = JavaConfig()
      )

      val result = Compiler.forConfig(input.config).compile(input)
      result shouldBe a[CompilationSuccess]

      val success = result.asInstanceOf[CompilationSuccess]
      success.compiledClasses should not be empty

      val mainClass = success.compiledClasses.find(_.toString.contains("Main"))
      mainClass shouldBe defined
      info(s"Compiled ${success.compiledClasses.size} class files including Main")
    } finally deleteRecursively(outputDir)
  }

  test("Java: run main class") {
    val outputDir = createTempDir("java-run-")
    try {
      val input = CompilationInput(
        sources = Seq(javaMainSource),
        classpath = Seq.empty,
        outputDir = outputDir,
        config = JavaConfig()
      )

      val result = Compiler.forConfig(input.config).compile(input)
      result shouldBe a[CompilationSuccess]

      // Run the main class
      val javaHome = System.getProperty("java.home")
      val javaBin = Path.of(javaHome, "bin", "java").toString

      val process = new ProcessBuilder(javaBin, "-cp", outputDir.toString, "example.Main", "java", "args")
        .redirectErrorStream(true)
        .start()

      val output = new String(process.getInputStream.readAllBytes())
      val exitCode = process.waitFor()

      exitCode shouldBe 0
      output should include("java args")
      output should include("Args count: 2")
      info(s"Main class ran successfully with output: ${output.trim}")
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // Scala Test Framework Tests
  // ============================================================================

  val scalaTestSource = SourceFile(
    Path.of("ExampleTest.scala"),
    """package example
      |
      |import org.scalatest.funsuite.AnyFunSuite
      |import org.scalatest.matchers.should.Matchers
      |
      |class ExampleTest extends AnyFunSuite with Matchers:
      |  test("addition works"):
      |    1 + 1 shouldBe 2
      |
      |  test("string operations"):
      |    "hello".length shouldBe 5
      |""".stripMargin
  )

  test("Scala: compile test class with ScalaTest") {
    val outputDir = createTempDir("scala-test-")
    try {
      val input = CompilationInput(
        sources = Seq(scalaTestSource),
        classpath = CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.scalaTestLibrary,
        outputDir = outputDir,
        config = ScalaConfig(version = "3.7.4")
      )

      val result = Compiler.forConfig(input.config).compile(input)
      result shouldBe a[CompilationSuccess]

      val success = result.asInstanceOf[CompilationSuccess]
      success.compiledClasses should not be empty

      val testClass = success.compiledClasses.find(_.toString.contains("ExampleTest"))
      testClass shouldBe defined
      info(s"Compiled test class: ${testClass.get}")
    } finally deleteRecursively(outputDir)
  }

  test("Scala: run tests with ScalaTest") {
    val outputDir = createTempDir("scala-test-run-")
    try {
      val input = CompilationInput(
        sources = Seq(scalaTestSource),
        classpath = CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.scalaTestLibrary,
        outputDir = outputDir,
        config = ScalaConfig(version = "3.7.4")
      )

      val result = Compiler.forConfig(input.config).compile(input)
      result shouldBe a[CompilationSuccess]

      // Run tests with ScalaTest Runner
      val classpath = (outputDir :: (CompilerTestLibraries.scalaLibrary ++ CompilerTestLibraries.scalaTestLibrary).toList)
        .map(_.toString)
        .mkString(java.io.File.pathSeparator)

      val javaHome = System.getProperty("java.home")
      val javaBin = Path.of(javaHome, "bin", "java").toString

      val process = new ProcessBuilder(
        javaBin,
        "-cp",
        classpath,
        "org.scalatest.tools.Runner",
        "-s",
        "example.ExampleTest",
        "-oD"
      )
        .redirectErrorStream(true)
        .start()

      val output = new String(process.getInputStream.readAllBytes())
      val exitCode = process.waitFor()

      exitCode shouldBe 0
      output should include("ExampleTest")
      info(s"Tests ran successfully")
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // Kotlin Test Framework Tests
  // ============================================================================

  val kotlinTestSource = SourceFile(
    Path.of("ExampleTest.kt"),
    """package example
      |
      |import kotlin.test.Test
      |import kotlin.test.assertEquals
      |
      |class ExampleTest {
      |    @Test
      |    fun `addition works`() {
      |        assertEquals(2, 1 + 1)
      |    }
      |
      |    @Test
      |    fun `string operations`() {
      |        assertEquals(5, "hello".length)
      |    }
      |}
      |""".stripMargin
  )

  test("Kotlin: compile test class with kotlin-test") {
    val outputDir = createTempDir("kotlin-test-")
    try {
      val input = CompilationInput(
        sources = Seq(kotlinTestSource),
        classpath = CompilerTestLibraries.kotlinLibrary ++ CompilerTestLibraries.kotlinTestLibrary,
        outputDir = outputDir,
        config = KotlinConfig(version = "2.3.0")
      )

      val result = KotlinSourceCompiler.compile(input)
      result shouldBe a[CompilationSuccess]

      val success = result.asInstanceOf[CompilationSuccess]
      success.compiledClasses should not be empty

      val testClass = success.compiledClasses.find(_.toString.contains("ExampleTest"))
      testClass shouldBe defined
      info(s"Compiled test class: ${testClass.get}")
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // Java Test Framework Tests
  // ============================================================================

  val javaTestSource = SourceFile(
    Path.of("example/ExampleTest.java"),
    """package example;
      |
      |import org.junit.Test;
      |import static org.junit.Assert.*;
      |
      |public class ExampleTest {
      |    @Test
      |    public void additionWorks() {
      |        assertEquals(2, 1 + 1);
      |    }
      |
      |    @Test
      |    public void stringOperations() {
      |        assertEquals(5, "hello".length());
      |    }
      |}
      |""".stripMargin
  )

  test("Java: compile test class with JUnit") {
    val outputDir = createTempDir("java-test-")
    try {
      val input = CompilationInput(
        sources = Seq(javaTestSource),
        classpath = CompilerTestLibraries.junitLibrary,
        outputDir = outputDir,
        config = JavaConfig()
      )

      val result = Compiler.forConfig(input.config).compile(input)
      result shouldBe a[CompilationSuccess]

      val success = result.asInstanceOf[CompilationSuccess]
      success.compiledClasses should not be empty

      val testClass = success.compiledClasses.find(_.toString.contains("ExampleTest"))
      testClass shouldBe defined
      info(s"Compiled test class: ${testClass.get}")
    } finally deleteRecursively(outputDir)
  }

  test("Java: run tests with JUnit") {
    val outputDir = createTempDir("java-test-run-")
    try {
      val input = CompilationInput(
        sources = Seq(javaTestSource),
        classpath = CompilerTestLibraries.junitLibrary,
        outputDir = outputDir,
        config = JavaConfig()
      )

      val result = Compiler.forConfig(input.config).compile(input)
      result shouldBe a[CompilationSuccess]

      // Run tests with JUnit Console Launcher
      val classpath = (outputDir :: CompilerTestLibraries.junitLibrary.toList ++ CompilerTestLibraries.junitPlatformLibrary.toList)
        .map(_.toString)
        .mkString(java.io.File.pathSeparator)

      val javaHome = System.getProperty("java.home")
      val javaBin = Path.of(javaHome, "bin", "java").toString

      val process = new ProcessBuilder(
        javaBin,
        "-cp",
        classpath,
        "org.junit.platform.console.ConsoleLauncher",
        "--select-class=example.ExampleTest",
        "--disable-banner"
      )
        .redirectErrorStream(true)
        .start()

      val output = new String(process.getInputStream.readAllBytes())
      val exitCode = process.waitFor()

      exitCode shouldBe 0
      output should include("2 tests successful")
      info(s"Tests ran successfully")
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // JUnit 5 (Jupiter) Test Framework Tests
  // ============================================================================

  val javaJunit5TestSource = SourceFile(
    Path.of("example/ExampleJunit5Test.java"),
    """package example;
      |
      |import org.junit.jupiter.api.Test;
      |import static org.junit.jupiter.api.Assertions.*;
      |
      |public class ExampleJunit5Test {
      |    @Test
      |    public void additionWorks() {
      |        assertEquals(2, 1 + 1);
      |    }
      |
      |    @Test
      |    public void stringOperations() {
      |        assertEquals(5, "hello".length());
      |    }
      |}
      |""".stripMargin
  )

  test("Java: compile test class with JUnit 5") {
    val outputDir = createTempDir("java-junit5-test-")
    try {
      val input = CompilationInput(
        sources = Seq(javaJunit5TestSource),
        classpath = CompilerTestLibraries.junit5Library,
        outputDir = outputDir,
        config = JavaConfig()
      )

      val result = Compiler.forConfig(input.config).compile(input)
      result shouldBe a[CompilationSuccess]

      val success = result.asInstanceOf[CompilationSuccess]
      success.compiledClasses should not be empty

      val testClass = success.compiledClasses.find(_.toString.contains("ExampleJunit5Test"))
      testClass shouldBe defined
      info(s"Compiled JUnit 5 test class: ${testClass.get}")
    } finally deleteRecursively(outputDir)
  }

  test("Java: run JUnit 5 tests with ForkedTestRunner") {
    val outputDir = createTempDir("java-junit5-run-")
    try {
      val input = CompilationInput(
        sources = Seq(javaJunit5TestSource),
        classpath = CompilerTestLibraries.junit5Library,
        outputDir = outputDir,
        config = JavaConfig()
      )

      val result = Compiler.forConfig(input.config).compile(input)
      result shouldBe a[CompilationSuccess]

      // Build classpath: compiled test classes + jupiter-interface + ForkedTestRunner
      val testRunnerLocation = classOf[bleep.testing.runner.ForkedTestRunner].getProtectionDomain.getCodeSource.getLocation.toURI
      val testRunnerPath = Path.of(testRunnerLocation)

      val classpathEntries = List(outputDir, testRunnerPath) ++
        CompilerTestLibraries.jupiterInterfaceLibrary.toList

      val classpath = classpathEntries
        .map(_.toString)
        .mkString(java.io.File.pathSeparator)

      val javaHome = System.getProperty("java.home")
      val javaBin = Path.of(javaHome, "bin", "java").toString

      val process = new ProcessBuilder(
        javaBin,
        "-cp",
        classpath,
        "bleep.testing.runner.ForkedTestRunner"
      )
        .start()

      val reader = new BufferedReader(new InputStreamReader(process.getInputStream))
      val writer = new PrintWriter(process.getOutputStream, true)
      val stderr = new BufferedReader(new InputStreamReader(process.getErrorStream))

      try {
        // Wait for Ready
        val readyLine = reader.readLine()
        readyLine should include("\"type\":\"Ready\"")

        // Send RunSuite command
        val command = bleep.testing.runner.TestProtocol.encodeRunSuite(
          "example.ExampleJunit5Test",
          "JUnit Jupiter",
          java.util.List.of()
        )
        writer.println(command)

        // Collect all protocol output until SuiteDone
        val lines = collectUntilSuiteDone(reader)

        // Send shutdown
        writer.println(bleep.testing.runner.TestProtocol.encodeShutdown())

        // Verify we got test results
        val testFinishedLines = lines.filter(_.contains("\"type\":\"TestFinished\""))
        val suiteDoneLines = lines.filter(_.contains("\"type\":\"SuiteDone\""))

        info(s"Protocol lines: ${lines.size}")
        lines.foreach(l => info(s"  $l"))

        testFinishedLines.size shouldBe 2
        testFinishedLines.foreach { line =>
          line should include("\"status\":\"passed\"")
        }

        suiteDoneLines.size shouldBe 1
        suiteDoneLines.head should include("\"passed\":2")
        suiteDoneLines.head should include("\"failed\":0")

        info("JUnit 5 tests ran successfully via ForkedTestRunner")
      } finally {
        process.destroyForcibly()
        reader.close()
        writer.close()
        stderr.close()
      }
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // JUnit 4 via ForkedTestRunner (vintage engine)
  // ============================================================================

  test("Java: run JUnit 4 tests with ForkedTestRunner via vintage engine") {
    val outputDir = createTempDir("java-junit4-forked-")
    try {
      val input = CompilationInput(
        sources = Seq(javaTestSource),
        classpath = CompilerTestLibraries.junitLibrary,
        outputDir = outputDir,
        config = JavaConfig()
      )

      val result = Compiler.forConfig(input.config).compile(input)
      result shouldBe a[CompilationSuccess]

      // Build classpath: compiled test classes + jupiter-interface (with vintage engine) + ForkedTestRunner
      val testRunnerLocation = classOf[bleep.testing.runner.ForkedTestRunner].getProtectionDomain.getCodeSource.getLocation.toURI
      val testRunnerPath = Path.of(testRunnerLocation)

      val classpathEntries = List(outputDir, testRunnerPath) ++
        CompilerTestLibraries.jupiterInterfaceLibrary.toList ++
        CompilerTestLibraries.junitLibrary.toList

      val classpath = classpathEntries
        .map(_.toString)
        .mkString(java.io.File.pathSeparator)

      val javaHome = System.getProperty("java.home")
      val javaBin = Path.of(javaHome, "bin", "java").toString

      val process = new ProcessBuilder(
        javaBin,
        "-cp",
        classpath,
        "bleep.testing.runner.ForkedTestRunner"
      )
        .start()

      val reader = new BufferedReader(new InputStreamReader(process.getInputStream))
      val writer = new PrintWriter(process.getOutputStream, true)
      val stderr = new BufferedReader(new InputStreamReader(process.getErrorStream))

      try {
        // Wait for Ready
        val readyLine = reader.readLine()
        readyLine should include("\"type\":\"Ready\"")

        // Send RunSuite command — framework name is "JUnit" (ForkedTestRunner
        // loads JupiterFramework first, which via vintage engine can run JUnit 4 tests)
        val command = bleep.testing.runner.TestProtocol.encodeRunSuite(
          "example.ExampleTest",
          "JUnit",
          java.util.List.of()
        )
        writer.println(command)

        // Collect all protocol output until SuiteDone
        val lines = collectUntilSuiteDone(reader)

        // Send shutdown
        writer.println(bleep.testing.runner.TestProtocol.encodeShutdown())

        // Verify we got test results
        val testFinishedLines = lines.filter(_.contains("\"type\":\"TestFinished\""))
        val suiteDoneLines = lines.filter(_.contains("\"type\":\"SuiteDone\""))

        info(s"Protocol lines: ${lines.size}")
        lines.foreach(l => info(s"  $l"))

        testFinishedLines.size shouldBe 2
        testFinishedLines.foreach { line =>
          line should include("\"status\":\"passed\"")
        }

        suiteDoneLines.size shouldBe 1
        suiteDoneLines.head should include("\"passed\":2")
        suiteDoneLines.head should include("\"failed\":0")

        info("JUnit 4 tests ran successfully via ForkedTestRunner (vintage engine)")
      } finally {
        process.destroyForcibly()
        reader.close()
        writer.close()
        stderr.close()
      }
    } finally deleteRecursively(outputDir)
  }
}

trait RunAndTestHelpers {

  /** Read protocol lines from ForkedTestRunner until we see a SuiteDone message */
  def collectUntilSuiteDone(reader: BufferedReader): List[String] = {
    val lines = ListBuffer[String]()
    var done = false
    while (!done) {
      val line = reader.readLine()
      if (line == null) {
        done = true
      } else {
        lines += line
        if (line.contains("\"type\":\"SuiteDone\"")) {
          done = true
        }
      }
    }
    lines.toList
  }
}
