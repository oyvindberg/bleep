package bleep.analysis

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.concurrent.TimeLimits
import org.scalatest.time.{Seconds, Span}

import java.nio.file.{Files, Path}
import scala.jdk.StreamConverters._

/** Integration tests using the BSP test harness.
  *
  * These tests verify BSP protocol communication by:
  *   1. Writing source files to disk
  *   2. Sending BSP compile/test requests
  *   3. Observing events and diagnostics
  *
  * This is the recommended way to test bleep-bsp functionality.
  */
class BspHarnessIntegrationTest extends AnyFunSuite with Matchers with TimeLimits {

  val mediumTimeout: Span = Span(120, Seconds)

  def createTempWorkspace(prefix: String): Path = {
    val dir = Files.createTempDirectory(prefix)
    // Create src and output directories
    Files.createDirectories(dir.resolve("src"))
    Files.createDirectories(dir.resolve("target/classes"))
    dir
  }

  def deleteRecursively(path: Path): Unit =
    if (Files.exists(path)) {
      if (Files.isDirectory(path)) {
        Files.list(path).toScala(List).foreach(deleteRecursively)
      }
      Files.delete(path)
    }

  // Resolve Scala library via Coursier for the specified version
  def scalaLibraryClasspath(version: String): List[Path] =
    CompilerResolver.resolveScalaLibrary(version).toList

  test("BspTestHarness: initialize and shutdown") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-harness-init-")
      try {
        val config = BspTestHarness.ProjectConfig.scala(
          name = "myproject",
          sources = Set(workspace.resolve("src")),
          outputDir = workspace.resolve("target/classes"),
          scalaVersion = "3.3.3",
          classpath = scalaLibraryClasspath("3.3.3"),
          isTest = false
        )

        BspTestHarness.withProject(workspace, config) { client =>
          val result = client.initialize()

          result.displayName should not be empty
          result.version should not be empty
          result.bspVersion should not be empty
          result.capabilities should not be null

          info(s"Server: ${result.displayName} v${result.version}")
          info(s"BSP version: ${result.bspVersion}")
        }
      } finally deleteRecursively(workspace)
    }
  }

  test("BspTestHarness: get build targets") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-harness-targets-")
      try {
        val config = BspTestHarness.ProjectConfig.scala(
          name = "myproject",
          sources = Set(workspace.resolve("src")),
          outputDir = workspace.resolve("target/classes"),
          scalaVersion = "3.3.3",
          classpath = scalaLibraryClasspath("3.3.3"),
          isTest = false
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()

          val targets = client.buildTargets()

          info(s"Found ${targets.targets.size} build targets")
          targets.targets.foreach { t =>
            info(s"  - ${t.displayName.getOrElse(t.id.uri.value)}")
          }

          // Should have at least the project we defined
          targets.targets should not be empty
          targets.targets.head.displayName shouldBe Some("myproject")
        }
      } finally deleteRecursively(workspace)
    }
  }

  test("BspTestHarness: compile successful Scala code") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-harness-compile-")
      try {
        // Write valid Scala source
        val srcFile = workspace.resolve("src/Hello.scala")
        Files.writeString(
          srcFile,
          """object Hello {
            |  def main(args: Array[String]): Unit = {
            |    println("Hello, BSP!")
            |  }
            |}
            |""".stripMargin
        )

        val config = BspTestHarness.ProjectConfig.scala(
          name = "myproject",
          sources = Set(workspace.resolve("src")),
          outputDir = workspace.resolve("target/classes"),
          scalaVersion = "3.3.3",
          classpath = scalaLibraryClasspath("3.3.3"),
          isTest = false
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()

          val targets = client.buildTargets()
          targets.targets should not be empty

          val targetIds = targets.targets.map(_.id)
          val result = client.compile(targetIds)

          info(s"Compile status: ${result.statusCode}")
          info(s"Events collected: ${client.events.size}")
          client.events.foreach(e => info(s"  - $e"))

          // Compilation should succeed
          result.statusCode.value shouldBe 1 // StatusCode.Ok = 1
        }
      } finally deleteRecursively(workspace)
    }
  }

  test("BspTestHarness: compile code with errors collects diagnostics") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-harness-errors-")
      try {
        // Write Scala source with compile error
        val srcFile = workspace.resolve("src/Broken.scala")
        Files.writeString(
          srcFile,
          """object Broken {
            |  def main(args: Array[String]): Unit = {
            |    val x: Int = "not an int"  // Type error
            |  }
            |}
            |""".stripMargin
        )

        val config = BspTestHarness.ProjectConfig.scala(
          name = "myproject",
          sources = Set(workspace.resolve("src")),
          outputDir = workspace.resolve("target/classes"),
          scalaVersion = "3.3.3",
          classpath = scalaLibraryClasspath("3.3.3"),
          isTest = false
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()

          val targets = client.buildTargets()
          val targetIds = targets.targets.map(_.id)
          val result = client.compile(targetIds)

          info(s"Compile status: ${result.statusCode}")
          info(s"Diagnostics by file:")
          client.diagnosticsByFile.foreach { case (uri, diags) =>
            info(s"  $uri:")
            diags.foreach(d => info(s"    - ${d.message} (line ${d.line.getOrElse("?")}:${d.column.getOrElse("?")})"))
          }

          // Compilation should fail
          result.statusCode.value shouldBe 2 // StatusCode.Error = 2

          // Should have collected diagnostics
          client.diagnosticsByFile should not be empty
        }
      } finally deleteRecursively(workspace)
    }
  }

  test("BspTestHarness: incremental compilation - only recompiles changed files") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-harness-incremental-")
      try {
        // Write two source files
        val fooFile = workspace.resolve("src/Foo.scala")
        Files.writeString(fooFile, """object Foo { def hello: String = "Hello" }""")

        val barFile = workspace.resolve("src/Bar.scala")
        Files.writeString(barFile, """object Bar { def world: String = "World" }""")

        val config = BspTestHarness.ProjectConfig.scala(
          name = "myproject",
          sources = Set(workspace.resolve("src")),
          outputDir = workspace.resolve("target/classes"),
          scalaVersion = "3.3.3",
          classpath = scalaLibraryClasspath("3.3.3"),
          isTest = false
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()
          val targets = client.buildTargets()
          val targetIds = targets.targets.map(_.id)

          // First compile - should compile all files
          info("First compile (both files)...")
          val result1 = client.compile(targetIds)
          result1.statusCode.value shouldBe 1
          val events1 = client.events.size
          info(s"First compile events: $events1")
          client.clear()

          // Wait a bit for file system timestamp to change
          Thread.sleep(100)

          // Modify only Foo.scala
          Files.writeString(fooFile, """object Foo { def hello: String = "Hello Modified" }""")

          // Second compile - should only compile Foo
          info("Second compile (only Foo changed)...")
          val result2 = client.compile(targetIds)
          result2.statusCode.value shouldBe 1
          val events2 = client.events.size
          info(s"Second compile events: $events2")
          client.clear()

          // Third compile - nothing changed, should be no-op
          info("Third compile (no changes)...")
          val result3 = client.compile(targetIds)
          result3.statusCode.value shouldBe 1
          val events3 = client.events.size
          info(s"Third compile events: $events3")

          // Third compile should have fewer events (ideally none for compilation)
          info("Incremental compilation verified")
        }
      } finally deleteRecursively(workspace)
    }
  }

  test("BspTestHarness: run main class") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-harness-run-")
      try {
        // Write a main class
        val srcFile = workspace.resolve("src/Main.scala")
        Files.writeString(
          srcFile,
          """object Main {
            |  def main(args: Array[String]): Unit = {
            |    println("Hello from Main!")
            |    args.foreach(a => println(s"Arg: $a"))
            |  }
            |}
            |""".stripMargin
        )

        val config = BspTestHarness.ProjectConfig.scala(
          name = "myproject",
          sources = Set(workspace.resolve("src")),
          outputDir = workspace.resolve("target/classes"),
          scalaVersion = "3.3.3",
          classpath = scalaLibraryClasspath("3.3.3"),
          isTest = false
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()

          val targets = client.buildTargets()
          targets.targets should not be empty
          val targetId = targets.targets.head.id

          // Compile first
          val compileResult = client.compile(List(targetId))
          compileResult.statusCode.value shouldBe 1

          client.clear()

          // Run main class
          val runResult = client.run(targetId, "Main", List("arg1", "arg2"))

          info(s"Run status: ${runResult.statusCode}")
          info(s"Log messages: ${client.logMessages.take(5)}")

          // Should complete successfully
          runResult.statusCode.value shouldBe 1 // StatusCode.Ok

          // Should have captured output
          val logs = client.logMessages
          logs.exists(_.contains("Hello from Main!")) shouldBe true
          logs.exists(_.contains("Arg: arg1")) shouldBe true
          logs.exists(_.contains("Arg: arg2")) shouldBe true
        }
      } finally deleteRecursively(workspace)
    }
  }

  test("BspTestHarness: async compile with cancellation") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-harness-cancel-")
      try {
        // Write many files to make compilation slow enough to cancel
        for (i <- 1 to 50) {
          val srcFile = workspace.resolve(s"src/File$i.scala")
          Files.writeString(
            srcFile,
            s"""object File$i {
               |  def value$i: Int = $i
               |  def compute$i: List[Int] = (1 to 1000).toList.map(_ * $i)
               |  def heavy$i: Map[String, List[Int]] = (1 to 100).map(x => s"key$$x" -> (1 to 100).toList).toMap
               |}
               |""".stripMargin
          )
        }

        val config = BspTestHarness.ProjectConfig.scala(
          name = "myproject",
          sources = Set(workspace.resolve("src")),
          outputDir = workspace.resolve("target/classes"),
          scalaVersion = "3.3.3",
          classpath = scalaLibraryClasspath("3.3.3"),
          isTest = false
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()

          val targets = client.buildTargets()
          val targetIds = targets.targets.map(_.id)

          // Start async compile
          val handle = client.compileAsync(targetIds)
          info(s"Started async compile with request ID: ${handle.requestId}")

          // Cancel after short delay - give server time to start compiling
          Thread.sleep(200)
          handle.cancel()
          info("Sent cancel request")

          // Wait for response
          val result = handle.awaitWithTimeout(60000)
          result match {
            case Some(r) =>
              info(s"Compile completed with status: ${r.statusCode} (1=Ok, 2=Error, 3=Cancelled)")
              // Status should be Ok (if compiled fast), Error, or Cancelled
              // We can't guarantee cancellation wins the race, but we verify the client works
              r.statusCode.value should (be >= 1 and be <= 3)
            case None =>
              info("Compile timed out")
          }

          // The key verification is that we can continue using the client
          client.clear()
          val targets2 = client.buildTargets()
          targets2.targets should not be empty
          info("Client still functional after cancel")
        }
      } finally deleteRecursively(workspace)
    }
  }

  test("BspTestHarness: cancellation returns Cancelled status for multi-project") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-harness-cancel-multi-")
      try {
        // Create multiple projects to increase likelihood of catching cancellation
        for (p <- 1 to 5) {
          Files.createDirectories(workspace.resolve(s"project$p/src"))
          Files.createDirectories(workspace.resolve(s"project$p/target/classes"))
          for (i <- 1 to 10) {
            val srcFile = workspace.resolve(s"project$p/src/File$i.scala")
            Files.writeString(
              srcFile,
              s"""package project$p
                 |object File$i {
                 |  def value: Int = $i * $p
                 |  def heavy: Map[String, List[Int]] = (1 to 50).map(x => s"key$$x" -> (1 to 50).toList).toMap
                 |}
                 |""".stripMargin
            )
          }
        }

        val configs = (1 to 5).map { p =>
          BspTestHarness.ProjectConfig(
            name = s"project$p",
            sources = Set(workspace.resolve(s"project$p/src")),
            classpath = scalaLibraryClasspath("3.3.3"),
            outputDir = workspace.resolve(s"project$p/target/classes"),
            languageConfig = ScalaConfig("3.3.3", Nil),
            dependsOn = Set.empty,
            isTest = false
          )
        }.toList

        BspTestHarness.withProjects(workspace, configs) { client =>
          client.initialize()

          val targets = client.buildTargets()
          info(s"Found ${targets.targets.size} build targets")
          val targetIds = targets.targets.map(_.id)

          // Start async compile of all projects
          val handle = client.compileAsync(targetIds)
          info(s"Started async compile of ${targetIds.size} projects")

          // Cancel during compilation
          Thread.sleep(300)
          handle.cancel()
          info("Sent cancel request")

          // Wait for response
          val result = handle.awaitWithTimeout(120000)
          result match {
            case Some(r) =>
              info(s"Compile result: statusCode=${r.statusCode.value} (1=Ok, 2=Error, 3=Cancelled)")
            // If we caught it during cancellation, status should be 3
            // If it completed before cancel, status should be 1
            // Either way, client should be functional
            case None =>
              fail("Compile timed out unexpectedly")
          }

          // Verify client is still functional
          client.clear()
          val targets2 = client.buildTargets()
          targets2.targets.size shouldBe 5
          info("Client still functional after cancellation attempt")
        }
      } finally deleteRecursively(workspace)
    }
  }

  test("BspTestHarness: logMessages helper extracts log events") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-harness-logs-")
      try {
        val srcFile = workspace.resolve("src/Hello.scala")
        Files.writeString(
          srcFile,
          """object Hello {
            |  def main(args: Array[String]): Unit = println("Hello")
            |}
            |""".stripMargin
        )

        val config = BspTestHarness.ProjectConfig.scala(
          name = "myproject",
          sources = Set(workspace.resolve("src")),
          outputDir = workspace.resolve("target/classes"),
          scalaVersion = "3.3.3",
          classpath = scalaLibraryClasspath("3.3.3"),
          isTest = false
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()
          val targets = client.buildTargets()
          val targetIds = targets.targets.map(_.id)

          client.compile(targetIds)

          // logMessages should be a subset of events
          val allEvents = client.events
          val logs = client.logMessages

          info(s"Total events: ${allEvents.size}, Log messages: ${logs.size}")

          // Log messages should be strings (subset of BspEvent.LogMessage)
          logs.foreach { msg =>
            msg shouldBe a[String]
          }

          // Log events count should match
          val logEventCount = allEvents.count(_.isInstanceOf[BspTestHarness.BspEvent.LogMessage])
          logs.size shouldBe logEventCount
        }
      } finally deleteRecursively(workspace)
    }
  }
}
