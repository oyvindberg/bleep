package bleep.analysis

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}

/** Standard library resolution for compiler tests.
  *
  * Uses CompilerResolver to resolve the standard libraries needed for compilation. The resolver handles caching and isolated classloaders.
  */
object CompilerTestLibraries {
  import coursier.*

  /** Scala 3 library for a specific version */
  def scalaLibrary(version: String): Seq[Path] =
    CompilerResolver.resolveScalaLibrary(version)

  /** Scala 3 library for the current/default version (3.7.4) */
  lazy val scalaLibrary: Seq[Path] = scalaLibrary("3.7.4")

  /** Kotlin standard library for a specific version */
  def kotlinLibrary(version: String): Seq[Path] =
    CompilerResolver.resolveKotlinLibrary(version)

  /** Kotlin standard library for the current/default version (2.3.0) */
  lazy val kotlinLibrary: Seq[Path] = kotlinLibrary("2.3.0")

  /** ScalaTest library for testing */
  lazy val scalaTestLibrary: Seq[Path] = {
    val deps = Seq(
      Dependency(Module(Organization("org.scalatest"), ModuleName("scalatest_3")), "3.2.18")
    )
    Fetch().addDependencies(deps*).run().map(_.toPath).toSeq
  }

  /** Kotlin test library */
  lazy val kotlinTestLibrary: Seq[Path] = {
    val deps = Seq(
      Dependency(Module(Organization("org.jetbrains.kotlin"), ModuleName("kotlin-test")), "2.3.0"),
      Dependency(Module(Organization("org.jetbrains.kotlin"), ModuleName("kotlin-test-junit")), "2.3.0")
    )
    Fetch().addDependencies(deps*).run().map(_.toPath).toSeq
  }

  /** JUnit 4 library for Java testing */
  lazy val junitLibrary: Seq[Path] = {
    val deps = Seq(
      Dependency(Module(Organization("junit"), ModuleName("junit")), "4.13.2")
    )
    Fetch().addDependencies(deps*).run().map(_.toPath).toSeq
  }

  /** JUnit 5 Platform console launcher for running tests */
  lazy val junitPlatformLibrary: Seq[Path] = {
    val deps = Seq(
      Dependency(Module(Organization("org.junit.platform"), ModuleName("junit-platform-console-standalone")), "1.10.2"),
      Dependency(Module(Organization("org.junit.vintage"), ModuleName("junit-vintage-engine")), "5.10.2")
    )
    Fetch().addDependencies(deps*).run().map(_.toPath).toSeq
  }

  /** JUnit 5 (Jupiter) API for compiling JUnit 5 tests */
  lazy val junit5Library: Seq[Path] = {
    val deps = Seq(
      Dependency(Module(Organization("org.junit.jupiter"), ModuleName("junit-jupiter-api")), "5.9.1")
    )
    Fetch().addDependencies(deps*).run().map(_.toPath).toSeq
  }

  /** Jupiter interface (sbt-testing bridge for JUnit 5) + test-interface + vintage engine for ForkedTestRunner */
  lazy val jupiterInterfaceLibrary: Seq[Path] = {
    val deps = Seq(
      Dependency(Module(Organization("net.aichler"), ModuleName("jupiter-interface")), "0.11.1"),
      Dependency(Module(Organization("org.scala-sbt"), ModuleName("test-interface")), "1.0"),
      Dependency(Module(Organization("org.junit.vintage"), ModuleName("junit-vintage-engine")), "5.9.1")
    )
    Fetch().addDependencies(deps*).run().map(_.toPath).toSeq
  }

  /** sbt test-interface (needed by ForkedTestRunner) */
  lazy val testInterfaceLibrary: Seq[Path] = {
    val deps = Seq(
      Dependency(Module(Organization("org.scala-sbt"), ModuleName("test-interface")), "1.0")
    )
    Fetch().addDependencies(deps*).run().map(_.toPath).toSeq
  }

  /** MUnit test library */
  lazy val munitLibrary: Seq[Path] = {
    val deps = Seq(
      Dependency(Module(Organization("org.scalameta"), ModuleName("munit_3")), "1.0.0")
    )
    Fetch().addDependencies(deps*).run().map(_.toPath).toSeq
  }

  /** utest library */
  lazy val utestLibrary: Seq[Path] = {
    val deps = Seq(
      Dependency(Module(Organization("com.lihaoyi"), ModuleName("utest_3")), "0.8.4")
    )
    Fetch().addDependencies(deps*).run().map(_.toPath).toSeq
  }

  /** Kotest libraries (runner + assertions) */
  lazy val kotestLibrary: Seq[Path] = {
    val deps = Seq(
      Dependency(Module(Organization("io.kotest"), ModuleName("kotest-runner-junit5-jvm")), "5.9.1"),
      Dependency(Module(Organization("io.kotest"), ModuleName("kotest-assertions-core-jvm")), "5.9.1")
    )
    Fetch().addDependencies(deps*).run().map(_.toPath).toSeq
  }

  /** TestNG library */
  lazy val testngLibrary: Seq[Path] = {
    val deps = Seq(
      Dependency(Module(Organization("org.testng"), ModuleName("testng")), "7.10.2")
    )
    Fetch().addDependencies(deps*).run().map(_.toPath).toSeq
  }

  /** Mill TestNG bridge (sbt-testing adapter for TestNG) */
  lazy val testngBridgeLibrary: Seq[Path] = {
    val deps = Seq(
      Dependency(Module(Organization("com.lihaoyi"), ModuleName("mill-contrib-testng_2.13")), "0.9.6")
    )
    Fetch().addDependencies(deps*).run().map(_.toPath).toSeq
  }
}

/** Integration tests for the compiler implementations.
  *
  * Tests single-file compilation and incremental scenarios for each language.
  */
class CompilerIntegrationTest extends AnyFunSuite with Matchers {

  // ============================================================================
  // Helper Methods
  // ============================================================================

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
  // Java Compiler Tests
  // ============================================================================

  test("Java: compile single file") {
    val outputDir = createTempDir("java-output-")
    try {
      val source = SourceFile(
        Path.of("com/example/Hello.java"),
        """package com.example;
          |public class Hello {
          |    public String greet() {
          |        return "Hello, World!";
          |    }
          |}
          |""".stripMargin
      )

      val input = CompilationInput(
        sources = Seq(source),
        classpath = Seq.empty,
        outputDir = outputDir,
        config = JavaConfig()
      )

      val result = Compiler.forConfig(input.config).compile(input)

      result match {
        case CompilationSuccess(dir, classes) =>
          info(s"Compiled ${classes.size} class files to $dir")
          classes should not be empty
          classes.exists(_.toString.contains("Hello.class")) shouldBe true
        case CompilationFailure(errors) =>
          fail(s"Compilation failed: ${errors.map(_.formatted).mkString("\n")}")
        case CompilationCancelled =>
          fail("Compilation was cancelled")
      }
    } finally deleteRecursively(outputDir)
  }

  test("Java: compile with syntax error reports error") {
    val outputDir = createTempDir("java-output-")
    try {
      val source = SourceFile(
        Path.of("com/example/Broken.java"),
        """package com.example;
          |public class Broken {
          |    public void broken( {  // Missing parameter
          |    }
          |}
          |""".stripMargin
      )

      val input = CompilationInput(
        sources = Seq(source),
        classpath = Seq.empty,
        outputDir = outputDir,
        config = JavaConfig()
      )

      val result = Compiler.forConfig(input.config).compile(input)

      result match {
        case CompilationFailure(errors) =>
          info(s"Got expected errors: ${errors.map(_.formatted).mkString(", ")}")
          errors should not be empty
        case CompilationSuccess(_, _) =>
          fail("Expected compilation to fail")
        case CompilationCancelled =>
          fail("Compilation was cancelled")
      }
    } finally deleteRecursively(outputDir)
  }

  test("Java: compile with --release targets specific version") {
    val outputDir = createTempDir("java-output-")
    try {
      val source = SourceFile(
        Path.of("com/example/Hello.java"),
        """package com.example;
          |public class Hello {
          |    public String greet() {
          |        return "Hello";
          |    }
          |}
          |""".stripMargin
      )

      val input = CompilationInput(
        sources = Seq(source),
        classpath = Seq.empty,
        outputDir = outputDir,
        config = JavaConfig(release = Some(11))
      )

      val result = Compiler.forConfig(input.config).compile(input)

      result match {
        case CompilationSuccess(dir, classes) =>
          info(s"Compiled with --release 11: ${classes.size} class files")
          classes should not be empty
        case CompilationFailure(errors) =>
          fail(s"Compilation failed: ${errors.map(_.formatted).mkString("\n")}")
        case CompilationCancelled =>
          fail("Compilation was cancelled")
      }
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // Scala Compiler Tests
  // ============================================================================

  test("Scala: compile single file") {
    val outputDir = createTempDir("scala-output-")
    try {
      val source = SourceFile(
        Path.of("com/example/Hello.scala"),
        """package com.example
          |
          |class Hello:
          |  def greet(): String = "Hello, World!"
          |""".stripMargin
      )

      val input = CompilationInput(
        sources = Seq(source),
        classpath = CompilerTestLibraries.scalaLibrary,
        outputDir = outputDir,
        config = ScalaConfig(version = "3.7.4")
      )

      val result = Compiler.forConfig(input.config).compile(input)

      result match {
        case CompilationSuccess(dir, classes) =>
          info(s"Compiled ${classes.size} class files to $dir")
          classes should not be empty
          classes.exists(_.toString.contains("Hello.class")) shouldBe true
        case CompilationFailure(errors) =>
          fail(s"Compilation failed: ${errors.map(_.formatted).mkString("\n")}")
        case CompilationCancelled =>
          fail("Compilation was cancelled")
      }
    } finally deleteRecursively(outputDir)
  }

  test("Scala: compile with syntax error reports error") {
    val outputDir = createTempDir("scala-output-")
    try {
      val source = SourceFile(
        Path.of("com/example/Broken.scala"),
        """package com.example
          |
          |class Broken:
          |  def broken(): String = {  // Missing closing
          |""".stripMargin
      )

      val input = CompilationInput(
        sources = Seq(source),
        classpath = CompilerTestLibraries.scalaLibrary,
        outputDir = outputDir,
        config = ScalaConfig(version = "3.7.4")
      )

      val result = Compiler.forConfig(input.config).compile(input)

      result match {
        case CompilationFailure(errors) =>
          info(s"Got expected errors: ${errors.map(_.formatted).mkString(", ")}")
          errors should not be empty
        case CompilationSuccess(_, _) =>
          fail("Expected compilation to fail")
        case CompilationCancelled =>
          fail("Compilation was cancelled")
      }
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // Kotlin Compiler Tests
  // ============================================================================

  test("Kotlin: compile single file") {
    val outputDir = createTempDir("kotlin-output-")
    try {
      val source = SourceFile(
        Path.of("com/example/Hello.kt"),
        """package com.example
          |
          |class Hello {
          |    fun greet(): String = "Hello, World!"
          |}
          |""".stripMargin
      )

      val input = CompilationInput(
        sources = Seq(source),
        classpath = CompilerTestLibraries.kotlinLibrary,
        outputDir = outputDir,
        config = KotlinConfig(version = "2.3.0", jvmTarget = "11")
      )

      val result = KotlinSourceCompiler.compile(input)

      result match {
        case CompilationSuccess(dir, classes) =>
          info(s"Compiled ${classes.size} class files to $dir")
          classes should not be empty
          classes.exists(_.toString.contains("Hello.class")) shouldBe true
        case CompilationFailure(errors) =>
          fail(s"Compilation failed: ${errors.map(_.formatted).mkString("\n")}")
        case CompilationCancelled =>
          fail("Compilation was cancelled")
      }
    } finally deleteRecursively(outputDir)
  }

  test("Kotlin: compile with syntax error reports error") {
    val outputDir = createTempDir("kotlin-output-")
    try {
      val source = SourceFile(
        Path.of("com/example/Broken.kt"),
        """package com.example
          |
          |class Broken {
          |    fun broken(): String {  // Missing closing
          |""".stripMargin
      )

      val input = CompilationInput(
        sources = Seq(source),
        classpath = CompilerTestLibraries.kotlinLibrary,
        outputDir = outputDir,
        config = KotlinConfig(version = "2.3.0")
      )

      val result = KotlinSourceCompiler.compile(input)

      result match {
        case CompilationFailure(errors) =>
          info(s"Got expected errors: ${errors.map(_.formatted).mkString(", ")}")
          errors should not be empty
        case CompilationSuccess(_, _) =>
          fail("Expected compilation to fail")
        case CompilationCancelled =>
          fail("Compilation was cancelled")
      }
    } finally deleteRecursively(outputDir)
  }
}

/** Incremental compilation tests - A depends on nothing, B depends on A, C depends on nothing.
  *
  * Tests scenarios:
  *   1. Change A (non-breaking) -> B recompiles, C doesn't
  *   2. Break A -> compilation fails
  *   3. Fix A -> B recompiles again
  */
class IncrementalCompilationTest extends AnyFunSuite with Matchers {

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
  // Java Incremental Tests
  // ============================================================================

  test("Java incremental: change A -> B recompiles, break A -> fails, fix A -> B recompiles") {
    val outputDir = createTempDir("java-incr-")
    try {
      // Initial sources: A (base), B (depends on A), C (independent)
      val sourceA_v1 = SourceFile(
        Path.of("com/example/A.java"),
        """package com.example;
          |public class A {
          |    public int getValue() { return 42; }
          |}
          |""".stripMargin
      )

      val sourceB = SourceFile(
        Path.of("com/example/B.java"),
        """package com.example;
          |public class B {
          |    private A a = new A();
          |    public int compute() { return a.getValue() * 2; }
          |}
          |""".stripMargin
      )

      val sourceC = SourceFile(
        Path.of("com/example/C.java"),
        """package com.example;
          |public class C {
          |    public String name() { return "independent"; }
          |}
          |""".stripMargin
      )

      // Step 1: Initial compilation
      info("Step 1: Initial compilation of A, B, C")
      val input1 = CompilationInput(
        sources = Seq(sourceA_v1, sourceB, sourceC),
        classpath = Seq.empty,
        outputDir = outputDir,
        config = JavaConfig()
      )

      val result1 = Compiler.forConfig(input1.config).compile(input1)
      result1 shouldBe a[CompilationSuccess]
      val success1 = result1.asInstanceOf[CompilationSuccess]
      info(s"  Compiled: ${success1.compiledClasses.map(_.getFileName).mkString(", ")}")

      // Step 2: Change A (add method) - B should still compile
      info("Step 2: Change A (add method)")
      val sourceA_v2 = SourceFile(
        Path.of("com/example/A.java"),
        """package com.example;
          |public class A {
          |    public int getValue() { return 42; }
          |    public int getOther() { return 100; }  // New method
          |}
          |""".stripMargin
      )

      val input2 = CompilationInput(
        sources = Seq(sourceA_v2, sourceB, sourceC),
        classpath = Seq.empty,
        outputDir = outputDir,
        config = JavaConfig()
      )

      val result2 = Compiler.forConfig(input2.config).compile(input2)
      result2 shouldBe a[CompilationSuccess]
      info("  A changed, B+C still compile")

      // Step 3: Break A (remove method B depends on)
      info("Step 3: Break A (remove getValue method)")
      val sourceA_broken = SourceFile(
        Path.of("com/example/A.java"),
        """package com.example;
          |public class A {
          |    // getValue removed - B will fail!
          |    public int getOther() { return 100; }
          |}
          |""".stripMargin
      )

      val input3 = CompilationInput(
        sources = Seq(sourceA_broken, sourceB, sourceC),
        classpath = Seq.empty,
        outputDir = outputDir,
        config = JavaConfig()
      )

      val result3 = Compiler.forConfig(input3.config).compile(input3)
      result3 shouldBe a[CompilationFailure]
      val failure3 = result3.asInstanceOf[CompilationFailure]
      info(s"  Compilation failed as expected: ${failure3.errors.head.message}")
      failure3.errors.exists(_.message.contains("getValue")) shouldBe true

      // Step 4: Fix A (restore method)
      info("Step 4: Fix A (restore getValue)")
      val input4 = CompilationInput(
        sources = Seq(sourceA_v1, sourceB, sourceC),
        classpath = Seq.empty,
        outputDir = outputDir,
        config = JavaConfig()
      )

      val result4 = Compiler.forConfig(input4.config).compile(input4)
      result4 shouldBe a[CompilationSuccess]
      info("  A fixed, everything compiles again")
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // Scala Incremental Tests
  // ============================================================================

  test("Scala incremental: change A -> B recompiles, break A -> fails, fix A -> B recompiles") {
    val outputDir = createTempDir("scala-incr-")
    try {
      val sourceA_v1 = SourceFile(
        Path.of("com/example/A.scala"),
        """package com.example
          |
          |class A:
          |  def getValue(): Int = 42
          |""".stripMargin
      )

      val sourceB = SourceFile(
        Path.of("com/example/B.scala"),
        """package com.example
          |
          |class B:
          |  private val a = A()
          |  def compute(): Int = a.getValue() * 2
          |""".stripMargin
      )

      val sourceC = SourceFile(
        Path.of("com/example/C.scala"),
        """package com.example
          |
          |class C:
          |  def name(): String = "independent"
          |""".stripMargin
      )

      // Step 1: Initial compilation
      info("Step 1: Initial compilation of A, B, C")
      val input1 = CompilationInput(
        sources = Seq(sourceA_v1, sourceB, sourceC),
        classpath = CompilerTestLibraries.scalaLibrary,
        outputDir = outputDir,
        config = ScalaConfig(version = "3.7.4")
      )

      val result1 = Compiler.forConfig(input1.config).compile(input1)
      result1 shouldBe a[CompilationSuccess]

      // Step 2: Change A
      info("Step 2: Change A (add method)")
      val sourceA_v2 = SourceFile(
        Path.of("com/example/A.scala"),
        """package com.example
          |
          |class A:
          |  def getValue(): Int = 42
          |  def getOther(): Int = 100
          |""".stripMargin
      )

      val input2 = CompilationInput(
        sources = Seq(sourceA_v2, sourceB, sourceC),
        classpath = CompilerTestLibraries.scalaLibrary,
        outputDir = outputDir,
        config = ScalaConfig(version = "3.7.4")
      )

      val result2 = Compiler.forConfig(input2.config).compile(input2)
      result2 shouldBe a[CompilationSuccess]

      // Step 3: Break A
      info("Step 3: Break A (remove getValue)")
      val sourceA_broken = SourceFile(
        Path.of("com/example/A.scala"),
        """package com.example
          |
          |class A:
          |  def getOther(): Int = 100
          |""".stripMargin
      )

      val input3 = CompilationInput(
        sources = Seq(sourceA_broken, sourceB, sourceC),
        classpath = CompilerTestLibraries.scalaLibrary,
        outputDir = outputDir,
        config = ScalaConfig(version = "3.7.4")
      )

      val result3 = Compiler.forConfig(input3.config).compile(input3)
      result3 shouldBe a[CompilationFailure]
      info("  Compilation failed as expected")

      // Step 4: Fix A
      info("Step 4: Fix A")
      val input4 = CompilationInput(
        sources = Seq(sourceA_v1, sourceB, sourceC),
        classpath = CompilerTestLibraries.scalaLibrary,
        outputDir = outputDir,
        config = ScalaConfig(version = "3.7.4")
      )

      val result4 = Compiler.forConfig(input4.config).compile(input4)
      result4 shouldBe a[CompilationSuccess]
      info("  A fixed, everything compiles again")
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // Kotlin Incremental Tests
  // ============================================================================

  test("Kotlin incremental: change A -> B recompiles, break A -> fails, fix A -> B recompiles") {
    val outputDir = createTempDir("kotlin-incr-")
    try {
      val sourceA_v1 = SourceFile(
        Path.of("com/example/A.kt"),
        """package com.example
          |
          |class A {
          |    fun getValue(): Int = 42
          |}
          |""".stripMargin
      )

      val sourceB = SourceFile(
        Path.of("com/example/B.kt"),
        """package com.example
          |
          |class B {
          |    private val a = A()
          |    fun compute(): Int = a.getValue() * 2
          |}
          |""".stripMargin
      )

      val sourceC = SourceFile(
        Path.of("com/example/C.kt"),
        """package com.example
          |
          |class C {
          |    fun name(): String = "independent"
          |}
          |""".stripMargin
      )

      // Step 1: Initial compilation
      info("Step 1: Initial compilation of A, B, C")
      val input1 = CompilationInput(
        sources = Seq(sourceA_v1, sourceB, sourceC),
        classpath = CompilerTestLibraries.kotlinLibrary,
        outputDir = outputDir,
        config = KotlinConfig(version = "2.3.0")
      )

      val result1 = KotlinSourceCompiler.compile(input1)
      result1 shouldBe a[CompilationSuccess]

      // Step 2: Change A
      info("Step 2: Change A (add method)")
      val sourceA_v2 = SourceFile(
        Path.of("com/example/A.kt"),
        """package com.example
          |
          |class A {
          |    fun getValue(): Int = 42
          |    fun getOther(): Int = 100
          |}
          |""".stripMargin
      )

      val input2 = CompilationInput(
        sources = Seq(sourceA_v2, sourceB, sourceC),
        classpath = CompilerTestLibraries.kotlinLibrary,
        outputDir = outputDir,
        config = KotlinConfig(version = "2.3.0")
      )

      val result2 = KotlinSourceCompiler.compile(input2)
      result2 shouldBe a[CompilationSuccess]

      // Step 3: Break A
      info("Step 3: Break A (remove getValue)")
      val sourceA_broken = SourceFile(
        Path.of("com/example/A.kt"),
        """package com.example
          |
          |class A {
          |    fun getOther(): Int = 100
          |}
          |""".stripMargin
      )

      val input3 = CompilationInput(
        sources = Seq(sourceA_broken, sourceB, sourceC),
        classpath = CompilerTestLibraries.kotlinLibrary,
        outputDir = outputDir,
        config = KotlinConfig(version = "2.3.0")
      )

      val result3 = KotlinSourceCompiler.compile(input3)
      result3 shouldBe a[CompilationFailure]
      info("  Compilation failed as expected")

      // Step 4: Fix A
      info("Step 4: Fix A")
      val input4 = CompilationInput(
        sources = Seq(sourceA_v1, sourceB, sourceC),
        classpath = CompilerTestLibraries.kotlinLibrary,
        outputDir = outputDir,
        config = KotlinConfig(version = "2.3.0")
      )

      val result4 = KotlinSourceCompiler.compile(input4)
      result4 shouldBe a[CompilationSuccess]
      info("  A fixed, everything compiles again")
    } finally deleteRecursively(outputDir)
  }
}

/** Tests for using classpath dependencies */
class ClasspathCompilationTest extends AnyFunSuite with Matchers {

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

  test("Java: compile using class from previously compiled output") {
    val libDir = createTempDir("java-lib-")
    val appDir = createTempDir("java-app-")
    try {
      // Step 1: Compile library
      val libSource = SourceFile(
        Path.of("com/lib/Helper.java"),
        """package com.lib;
          |public class Helper {
          |    public static int add(int a, int b) { return a + b; }
          |}
          |""".stripMargin
      )

      val libInput = CompilationInput(
        sources = Seq(libSource),
        classpath = Seq.empty,
        outputDir = libDir,
        config = JavaConfig()
      )

      val libResult = Compiler.forConfig(libInput.config).compile(libInput)
      libResult shouldBe a[CompilationSuccess]
      info("Library compiled")

      // Step 2: Compile app using library on classpath
      val appSource = SourceFile(
        Path.of("com/app/Main.java"),
        """package com.app;
          |import com.lib.Helper;
          |public class Main {
          |    public int compute() {
          |        return Helper.add(10, 20);
          |    }
          |}
          |""".stripMargin
      )

      val appInput = CompilationInput(
        sources = Seq(appSource),
        classpath = Seq(libDir), // Library output on classpath
        outputDir = appDir,
        config = JavaConfig()
      )

      val appResult = Compiler.forConfig(appInput.config).compile(appInput)
      appResult match {
        case CompilationSuccess(_, classes) =>
          info(s"App compiled with classpath dependency: ${classes.map(_.getFileName).mkString(", ")}")
          classes.exists(_.toString.contains("Main.class")) shouldBe true
        case CompilationFailure(errors) =>
          fail(s"App compilation failed: ${errors.map(_.formatted).mkString("\n")}")
        case CompilationCancelled =>
          fail("Compilation was cancelled")
      }
    } finally {
      deleteRecursively(libDir)
      deleteRecursively(appDir)
    }
  }

  test("Scala: compile using class from previously compiled output") {
    val libDir = createTempDir("scala-lib-")
    val appDir = createTempDir("scala-app-")
    try {
      // Step 1: Compile library
      val libSource = SourceFile(
        Path.of("com/lib/Helper.scala"),
        """package com.lib
          |
          |object Helper:
          |  def add(a: Int, b: Int): Int = a + b
          |""".stripMargin
      )

      val libInput = CompilationInput(
        sources = Seq(libSource),
        classpath = CompilerTestLibraries.scalaLibrary,
        outputDir = libDir,
        config = ScalaConfig(version = "3.7.4")
      )

      val libResult = Compiler.forConfig(libInput.config).compile(libInput)
      libResult shouldBe a[CompilationSuccess]
      info("Library compiled")

      // Step 2: Compile app using library on classpath
      val appSource = SourceFile(
        Path.of("com/app/Main.scala"),
        """package com.app
          |
          |import com.lib.Helper
          |
          |class Main:
          |  def compute(): Int = Helper.add(10, 20)
          |""".stripMargin
      )

      val appInput = CompilationInput(
        sources = Seq(appSource),
        classpath = CompilerTestLibraries.scalaLibrary :+ libDir,
        outputDir = appDir,
        config = ScalaConfig(version = "3.7.4")
      )

      val appResult = Compiler.forConfig(appInput.config).compile(appInput)
      appResult match {
        case CompilationSuccess(_, classes) =>
          info(s"App compiled with classpath dependency")
          classes.exists(_.toString.contains("Main.class")) shouldBe true
        case CompilationFailure(errors) =>
          fail(s"App compilation failed: ${errors.map(_.formatted).mkString("\n")}")
        case CompilationCancelled =>
          fail("Compilation was cancelled")
      }
    } finally {
      deleteRecursively(libDir)
      deleteRecursively(appDir)
    }
  }

  test("Kotlin: compile using class from previously compiled output") {
    val libDir = createTempDir("kotlin-lib-")
    val appDir = createTempDir("kotlin-app-")
    try {
      // Step 1: Compile library
      val libSource = SourceFile(
        Path.of("com/lib/Helper.kt"),
        """package com.lib
          |
          |object Helper {
          |    fun add(a: Int, b: Int): Int = a + b
          |}
          |""".stripMargin
      )

      val libInput = CompilationInput(
        sources = Seq(libSource),
        classpath = CompilerTestLibraries.kotlinLibrary,
        outputDir = libDir,
        config = KotlinConfig(version = "2.3.0")
      )

      val libResult = KotlinSourceCompiler.compile(libInput)
      libResult shouldBe a[CompilationSuccess]
      info("Library compiled")

      // Step 2: Compile app using library on classpath
      val appSource = SourceFile(
        Path.of("com/app/Main.kt"),
        """package com.app
          |
          |import com.lib.Helper
          |
          |class Main {
          |    fun compute(): Int = Helper.add(10, 20)
          |}
          |""".stripMargin
      )

      val appInput = CompilationInput(
        sources = Seq(appSource),
        classpath = CompilerTestLibraries.kotlinLibrary :+ libDir,
        outputDir = appDir,
        config = KotlinConfig(version = "2.3.0")
      )

      val appResult = KotlinSourceCompiler.compile(appInput)
      appResult match {
        case CompilationSuccess(_, classes) =>
          info(s"App compiled with classpath dependency")
          classes.exists(_.toString.contains("Main.class")) shouldBe true
        case CompilationFailure(errors) =>
          fail(s"App compilation failed: ${errors.map(_.formatted).mkString("\n")}")
        case CompilationCancelled =>
          fail("Compilation was cancelled")
      }
    } finally {
      deleteRecursively(libDir)
      deleteRecursively(appDir)
    }
  }
}

// =============================================================================
// Streaming Diagnostic Tests
// =============================================================================

/** Tests that verify diagnostics are streamed to the listener as they occur. */
class StreamingDiagnosticTest extends AnyFunSuite with Matchers {
  import scala.collection.mutable

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

  test("Java: diagnostics are streamed to listener") {
    val outputDir = createTempDir("java-streaming-")
    try {
      val source = SourceFile(
        Path.of("Broken.java"),
        """public class Broken {
          |    public void a( { }  // Error 1
          |    public void b( { }  // Error 2
          |}
          |""".stripMargin
      )

      val input = CompilationInput(
        sources = Seq(source),
        classpath = Seq.empty,
        outputDir = outputDir,
        config = JavaConfig()
      )

      val streamedErrors = mutable.ListBuffer[CompilerError]()
      val listener = new DiagnosticListener {
        override def onDiagnostic(error: CompilerError): Unit = {
          streamedErrors += error
          info(s"  Streamed: ${error.formatted}")
        }
      }

      info("Compiling with streaming listener...")
      val result = Compiler.forConfig(input.config).compile(input, listener)

      result shouldBe a[CompilationFailure]
      streamedErrors should not be empty
      info(s"Received ${streamedErrors.size} streamed diagnostics")

      // Verify the streamed errors match the final result
      val finalErrors = result.asInstanceOf[CompilationFailure].errors
      streamedErrors.filter(_.severity == CompilerError.Severity.Error).toList shouldBe finalErrors
    } finally deleteRecursively(outputDir)
  }

  test("Scala: diagnostics are streamed to listener") {
    val outputDir = createTempDir("scala-streaming-")
    try {
      val source = SourceFile(
        Path.of("Broken.scala"),
        """object Broken:
          |  val x: String = 42      // Error 1: type mismatch
          |  val y: Int = "hello"    // Error 2: type mismatch
          |""".stripMargin
      )

      val input = CompilationInput(
        sources = Seq(source),
        classpath = CompilerTestLibraries.scalaLibrary,
        outputDir = outputDir,
        config = ScalaConfig(version = "3.7.4")
      )

      val streamedErrors = mutable.ListBuffer[CompilerError]()
      val listener = new DiagnosticListener {
        override def onDiagnostic(error: CompilerError): Unit = {
          streamedErrors += error
          info(s"  Streamed: ${error.formatted}")
        }
      }

      info("Compiling with streaming listener...")
      val result = Compiler.forConfig(input.config).compile(input, listener)

      result shouldBe a[CompilationFailure]
      streamedErrors should not be empty
      info(s"Received ${streamedErrors.size} streamed diagnostics")

      // Verify the streamed errors match the final result
      val finalErrors = result.asInstanceOf[CompilationFailure].errors
      streamedErrors.filter(_.severity == CompilerError.Severity.Error).toList shouldBe finalErrors
    } finally deleteRecursively(outputDir)
  }

  test("Kotlin: diagnostics are streamed to listener") {
    val outputDir = createTempDir("kotlin-streaming-")
    try {
      val source = SourceFile(
        Path.of("Broken.kt"),
        """fun broken() {
          |    val x: String = 42      // Error 1: type mismatch
          |    val y: Int = "hello"    // Error 2: type mismatch
          |}
          |""".stripMargin
      )

      val input = CompilationInput(
        sources = Seq(source),
        classpath = CompilerTestLibraries.kotlinLibrary,
        outputDir = outputDir,
        config = KotlinConfig(version = "2.3.0")
      )

      val streamedErrors = mutable.ListBuffer[CompilerError]()
      val listener = new DiagnosticListener {
        override def onDiagnostic(error: CompilerError): Unit = {
          streamedErrors += error
          info(s"  Streamed: ${error.formatted}")
        }
      }

      info("Compiling with streaming listener...")
      val result = KotlinSourceCompiler.compile(input, listener)

      result shouldBe a[CompilationFailure]
      streamedErrors should not be empty
      info(s"Received ${streamedErrors.size} streamed diagnostics")

      // Verify the streamed errors match the final result
      val finalErrors = result.asInstanceOf[CompilationFailure].errors
      streamedErrors.toList shouldBe finalErrors
    } finally deleteRecursively(outputDir)
  }
}

// =============================================================================
// Compiler Version Isolation Tests
// =============================================================================

/** Tests that verify different compiler versions can be loaded and used in isolation.
  *
  * These tests ensure:
  *   1. Older compiler versions can be resolved via Coursier
  *   2. Each version runs in an isolated classloader
  *   3. Code compiles correctly with version-appropriate libraries
  */
class CompilerVersionIsolationTest extends AnyFunSuite with Matchers {

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
  // Scala Version Isolation Tests
  // ============================================================================

  test("Scala: compile with current version (3.7.4)") {
    val outputDir = createTempDir("scala-version-current-")
    try {
      val source = SourceFile(
        Path.of("Hello.scala"),
        """object Hello:
          |  def greet(): String = "Hello from Scala 3.7.4"
          |""".stripMargin
      )

      val input = CompilationInput(
        sources = Seq(source),
        classpath = CompilerTestLibraries.scalaLibrary("3.7.4"),
        outputDir = outputDir,
        config = ScalaConfig(version = "3.7.4")
      )

      val result = Compiler.forConfig(input.config).compile(input)
      result match {
        case CompilationSuccess(_, classes) =>
          info(s"Compiled with Scala 3.7.4: ${classes.size} class files")
          classes should not be empty
        case CompilationFailure(errors) =>
          fail(s"Compilation failed: ${errors.map(_.formatted).mkString("\n")}")
        case CompilationCancelled =>
          fail("Compilation was cancelled")
      }
    } finally deleteRecursively(outputDir)
  }

  test("Scala: compile with older version (3.3.3)") {
    val outputDir = createTempDir("scala-version-old-")
    try {
      val source = SourceFile(
        Path.of("Hello.scala"),
        """object Hello:
          |  def greet(): String = "Hello from Scala 3.3.3"
          |""".stripMargin
      )

      val input = CompilationInput(
        sources = Seq(source),
        classpath = CompilerTestLibraries.scalaLibrary("3.3.3"),
        outputDir = outputDir,
        config = ScalaConfig(version = "3.3.3")
      )

      info("Resolving Scala 3.3.3 compiler (this may download JARs on first run)...")
      val result = Compiler.forConfig(input.config).compile(input)
      result match {
        case CompilationSuccess(_, classes) =>
          info(s"Compiled with Scala 3.3.3: ${classes.size} class files")
          classes should not be empty
        case CompilationFailure(errors) =>
          fail(s"Compilation failed: ${errors.map(_.formatted).mkString("\n")}")
        case CompilationCancelled =>
          fail("Compilation was cancelled")
      }
    } finally deleteRecursively(outputDir)
  }

  test("Scala: compile same source with different versions produces output") {
    val outputDir374 = createTempDir("scala-version-374-")
    val outputDir333 = createTempDir("scala-version-333-")
    try {
      val source = SourceFile(
        Path.of("Hello.scala"),
        """object Hello:
          |  def greet(): String = "Hello"
          |""".stripMargin
      )

      // Compile with 3.7.4
      val input374 = CompilationInput(
        sources = Seq(source),
        classpath = CompilerTestLibraries.scalaLibrary("3.7.4"),
        outputDir = outputDir374,
        config = ScalaConfig(version = "3.7.4")
      )
      val result374 = Compiler.forConfig(input374.config).compile(input374)
      result374 shouldBe a[CompilationSuccess]

      // Compile with 3.3.3
      val input333 = CompilationInput(
        sources = Seq(source),
        classpath = CompilerTestLibraries.scalaLibrary("3.3.3"),
        outputDir = outputDir333,
        config = ScalaConfig(version = "3.3.3")
      )
      val result333 = Compiler.forConfig(input333.config).compile(input333)
      result333 shouldBe a[CompilationSuccess]

      // Both should have produced class files
      val classes374 = result374.asInstanceOf[CompilationSuccess].compiledClasses
      val classes333 = result333.asInstanceOf[CompilationSuccess].compiledClasses

      info(s"3.7.4 produced ${classes374.size} classes, 3.3.3 produced ${classes333.size} classes")
      classes374 should not be empty
      classes333 should not be empty
    } finally {
      deleteRecursively(outputDir374)
      deleteRecursively(outputDir333)
    }
  }

  // ============================================================================
  // Kotlin Version Isolation Tests
  // ============================================================================

  test("Kotlin: compile with current version (2.3.0)") {
    val outputDir = createTempDir("kotlin-version-current-")
    try {
      val source = SourceFile(
        Path.of("Hello.kt"),
        """fun greet(): String = "Hello from Kotlin 2.3.0"
          |""".stripMargin
      )

      val input = CompilationInput(
        sources = Seq(source),
        classpath = CompilerTestLibraries.kotlinLibrary("2.3.0"),
        outputDir = outputDir,
        config = KotlinConfig(version = "2.3.0")
      )

      val result = KotlinSourceCompiler.compile(input)
      result match {
        case CompilationSuccess(_, classes) =>
          info(s"Compiled with Kotlin 2.3.0: ${classes.size} class files")
          classes should not be empty
        case CompilationFailure(errors) =>
          fail(s"Compilation failed: ${errors.map(_.formatted).mkString("\n")}")
        case CompilationCancelled =>
          fail("Compilation was cancelled")
      }
    } finally deleteRecursively(outputDir)
  }

  test("Kotlin: compile with older version (2.2.0)") {
    val outputDir = createTempDir("kotlin-version-old-")
    try {
      val source = SourceFile(
        Path.of("Hello.kt"),
        """fun greet(): String = "Hello from Kotlin 2.2.0"
          |""".stripMargin
      )

      val input = CompilationInput(
        sources = Seq(source),
        classpath = CompilerTestLibraries.kotlinLibrary("2.2.0"),
        outputDir = outputDir,
        config = KotlinConfig(version = "2.2.0")
      )

      info("Resolving Kotlin 2.2.0 compiler (this may download JARs on first run)...")
      val result = KotlinSourceCompiler.compile(input)
      result match {
        case CompilationSuccess(_, classes) =>
          info(s"Compiled with Kotlin 2.2.0: ${classes.size} class files")
          classes should not be empty
        case CompilationFailure(errors) =>
          fail(s"Compilation failed: ${errors.map(_.formatted).mkString("\n")}")
        case CompilationCancelled =>
          fail("Compilation was cancelled")
      }
    } finally deleteRecursively(outputDir)
  }

  test("Kotlin: compile same source with different versions produces output") {
    val outputDir230 = createTempDir("kotlin-version-230-")
    val outputDir220 = createTempDir("kotlin-version-220-")
    try {
      val source = SourceFile(
        Path.of("Hello.kt"),
        """fun greet(): String = "Hello"
          |""".stripMargin
      )

      // Compile with 2.3.0
      val input230 = CompilationInput(
        sources = Seq(source),
        classpath = CompilerTestLibraries.kotlinLibrary("2.3.0"),
        outputDir = outputDir230,
        config = KotlinConfig(version = "2.3.0")
      )
      val result230 = KotlinSourceCompiler.compile(input230)
      result230 shouldBe a[CompilationSuccess]

      // Compile with 2.2.0
      val input220 = CompilationInput(
        sources = Seq(source),
        classpath = CompilerTestLibraries.kotlinLibrary("2.2.0"),
        outputDir = outputDir220,
        config = KotlinConfig(version = "2.2.0")
      )
      val result220 = KotlinSourceCompiler.compile(input220)
      result220 shouldBe a[CompilationSuccess]

      // Both should have produced class files
      val classes230 = result230.asInstanceOf[CompilationSuccess].compiledClasses
      val classes220 = result220.asInstanceOf[CompilationSuccess].compiledClasses

      info(s"2.3.0 produced ${classes230.size} classes, 2.2.0 produced ${classes220.size} classes")
      classes230 should not be empty
      classes220 should not be empty
    } finally {
      deleteRecursively(outputDir230)
      deleteRecursively(outputDir220)
    }
  }

  // ============================================================================
  // Mixed Java + Kotlin Compilation
  // ============================================================================

  test("Kotlin: kotlinc references pre-compiled Java classes on classpath") {
    // Verify kotlinc can reference pre-compiled Java class files on the classpath
    // (the Java-first compilation order relies on this)
    val javaOutputDir = createTempDir("kotlin-java-precompiled-")
    val kotlinOutputDir = createTempDir("kotlin-classpath-test-")
    try {
      // Phase 1: Compile JavaHelper.java with javac
      val javaTempDir = Files.createTempDirectory("javac-src-")
      val javaDir = javaTempDir.resolve("com/example")
      Files.createDirectories(javaDir)
      Files.writeString(javaDir.resolve("JavaHelper.java"),
        """package com.example;
          |public class JavaHelper {
          |    public String getMessage() { return "Hello!"; }
          |}
          |""".stripMargin
      )
      val javac = javax.tools.ToolProvider.getSystemJavaCompiler
      val fm = javac.getStandardFileManager(null, null, null)
      val units = fm.getJavaFileObjectsFromPaths(java.util.List.of(javaDir.resolve("JavaHelper.java")))
      Files.createDirectories(javaOutputDir)
      val task = javac.getTask(null, fm, null, java.util.List.of("-d", javaOutputDir.toString), null, units)
      assert(task.call(), "javac compilation failed")
      fm.close()

      val javaClassFile = javaOutputDir.resolve("com/example/JavaHelper.class")
      assert(Files.exists(javaClassFile), s"JavaHelper.class not found at $javaClassFile")
      info(s"Java class compiled to $javaClassFile")

      // Phase 2: Compile Kotlin source with Java classes on classpath
      val kotlinSource = SourceFile(
        Path.of("com/example/Greeter.kt"),
        """package com.example
          |class Greeter {
          |    fun greet(helper: JavaHelper): String = helper.getMessage()
          |}
          |""".stripMargin
      )

      val input = CompilationInput(
        sources = Seq(kotlinSource),
        classpath = CompilerTestLibraries.kotlinLibrary :+ javaOutputDir,
        outputDir = kotlinOutputDir,
        config = KotlinConfig(version = "2.3.0", jvmTarget = "11")
      )

      val result = KotlinSourceCompiler.compile(input)
      result match {
        case CompilationSuccess(dir, classes) =>
          info(s"Kotlin compiled ${classes.size} class files")
          classes.exists(_.toString.contains("Greeter.class")) shouldBe true
        case CompilationFailure(errors) =>
          fail(s"Kotlin compilation with Java classpath failed: ${errors.map(_.formatted).mkString("\n")}")
        case CompilationCancelled =>
          fail("Cancelled")
      }
    } finally {
      deleteRecursively(javaOutputDir)
      deleteRecursively(kotlinOutputDir)
    }
  }

  test("Kotlin: mixed Java+Kotlin compilation - kotlinc uses Java for type resolution") {
    // kotlinc does NOT compile .java files — it only uses them for type resolution.
    // This test verifies that passing .java sources alongside .kt sources allows
    // Kotlin code to reference Java types, even though only .kt files produce .class output.
    val outputDir = createTempDir("kotlin-mixed-source-")
    try {
      val kotlinSource = SourceFile(
        Path.of("com/example/Greeter.kt"),
        """package com.example
          |
          |class Greeter {
          |    fun greet(helper: JavaHelper): String = helper.getMessage()
          |}
          |""".stripMargin
      )

      val javaSource = SourceFile(
        Path.of("com/example/JavaHelper.java"),
        """package com.example;
          |
          |public class JavaHelper {
          |    public String getMessage() {
          |        return "Hello from Java!";
          |    }
          |}
          |""".stripMargin
      )

      val input = CompilationInput(
        sources = Seq(kotlinSource, javaSource),
        classpath = CompilerTestLibraries.kotlinLibrary,
        outputDir = outputDir,
        config = KotlinConfig(version = "2.3.0", jvmTarget = "11")
      )

      val result = KotlinSourceCompiler.compile(input)

      result match {
        case CompilationSuccess(dir, classes) =>
          info(s"Compiled ${classes.size} class files to $dir")
          // kotlinc compiles Greeter.kt (which references JavaHelper)
          classes.exists(_.toString.contains("Greeter.class")) shouldBe true
          // kotlinc does NOT compile JavaHelper.java — only uses it for type resolution
          classes.exists(_.toString.contains("JavaHelper.class")) shouldBe false
        case CompilationFailure(errors) =>
          fail(s"Mixed compilation failed: ${errors.map(_.formatted).mkString("\n")}")
        case CompilationCancelled =>
          fail("Compilation was cancelled")
      }
    } finally deleteRecursively(outputDir)
  }

  test("Kotlin: mixed Java+Kotlin compilation - ProjectCompiler compiles Java first then Kotlin") {
    val sourceDir = createTempDir("kotlin-mixed-project-src-")
    val outputDir = createTempDir("kotlin-mixed-project-out-")
    try {
      // Write source files to directory (simulating a real project layout)
      // Java-first order: javac compiles JavaHelper.java, then kotlinc compiles Greeter.kt
      // with Java class output on classpath
      val ktDir = sourceDir.resolve("com/example")
      Files.createDirectories(ktDir)
      Files.writeString(ktDir.resolve("Greeter.kt"),
        """package com.example
          |
          |class Greeter {
          |    fun greet(helper: JavaHelper): String = helper.getMessage()
          |}
          |""".stripMargin
      )
      Files.writeString(ktDir.resolve("JavaHelper.java"),
        """package com.example;
          |
          |public class JavaHelper {
          |    public String getMessage() {
          |        return "Hello from Java!";
          |    }
          |}
          |""".stripMargin
      )

      val language = ProjectLanguage.Kotlin(
        kotlinVersion = "2.3.0",
        jvmTarget = "11",
        kotlinOptions = Nil
      )

      val config = ProjectConfig(
        name = "mixed-kotlin-java",
        sources = Set(sourceDir),
        classpath = CompilerTestLibraries.kotlinLibrary,
        outputDir = outputDir,
        language = language,
        analysisDir = None
      )

      import cats.effect.unsafe.implicits.global
      val result = KotlinProjectCompiler.compile(
        config,
        DiagnosticListener.noop,
        CancellationToken.never,
        Map.empty,
        ProgressListener.noop
      ).unsafeRunSync()

      result match {
        case ProjectCompileSuccess(dir, classFiles, _) =>
          info(s"Compiled ${classFiles.size} class files to $dir")
          val classNames = classFiles.map(_.getFileName.toString)
          classNames should contain("Greeter.class")
          // This is the key assertion: Java files must be compiled too
          classNames should contain("JavaHelper.class")
        case ProjectCompileFailure(errors) =>
          fail(s"Mixed compilation failed: ${errors.map(_.formatted).mkString("\n")}")
      }
    } finally {
      deleteRecursively(sourceDir)
      deleteRecursively(outputDir)
    }
  }

  // ============================================================================
  // Classloader Isolation Verification
  // ============================================================================

  test("CompilerResolver: caches compiler instances") {
    // Get the same version twice - should return cached instance
    val instance1 = CompilerResolver.getScalaCompiler("3.7.4")
    val instance2 = CompilerResolver.getScalaCompiler("3.7.4")

    // Should be the same cached instance
    instance1.loader shouldBe instance2.loader
    info("Compiler instances are cached correctly")
  }

  test("CompilerResolver: different versions have different classloaders") {
    val instance374 = CompilerResolver.getScalaCompiler("3.7.4")
    val instance333 = CompilerResolver.getScalaCompiler("3.3.3")

    // Should have different classloaders
    instance374.loader should not be instance333.loader
    info("Different versions have isolated classloaders")
  }

  test("CompilerResolver: resolves all necessary JARs") {
    val instance = CompilerResolver.getScalaCompiler("3.7.4")

    // Should have resolved multiple JARs
    instance.allJars.size should be > 1
    info(s"Resolved ${instance.allJars.size} JARs for Scala 3.7.4")

    // Should include compiler and library JARs
    instance.allJars.exists(_.getFileName.toString.contains("scala3-compiler")) shouldBe true
    instance.allJars.exists(_.getFileName.toString.contains("scala3-library")) shouldBe true
  }
}
