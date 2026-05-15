package bleep

import bleep.internal.FileUtils

import java.nio.file.Files

/** End-to-end ITs that drive bleep's KSP pipeline with the in-tree toy processor (`bleep-test-ksp-processor`).
  *
  * The toy processor is a tiny SymbolProcessor that recognises three annotations and emits deterministic outputs:
  *
  *   - `@GenerateKotlin(suffix)` → writes a sibling `.kt` class
  *   - `@GenerateJava(suffix)` → writes a sibling `.java` class (drives the KSP→javac mixed-compile path)
  *   - `@ThrowOnMe` → makes `process()` throw, exercising the processor-error path
  *
  * The processor is wired in as a bleep project (`bleep-test-ksp-processor`) and referenced by tests via
  * `build.bleep:bleep-test-ksp-processor:${BLEEP_VERSION}` which the dev-deps shim resolves to the project's compiled classes + `src/resources` (so the
  * META-INF/services SPI registration is visible).
  *
  * What this suite covers that the Moshi-based `KspIncrementalIT` cannot:
  *   - KSP emits `.java`, that `.java` is then compiled by javac in the same project compile.
  *   - User Kotlin and User Java both reference the generated symbol.
  *   - `@ThrowOnMe` surfaces as a compile failure.
  */
class KspToyProcessorIT extends IntegrationTestHarness {

  private val kotlinVersion = "2.1.20"
  private val kspVersion = "1.0.32"
  private val toy = "build.bleep:bleep-test-ksp-processor:${BLEEP_VERSION}"

  private def projectName: model.CrossProjectName = model.CrossProjectName(model.ProjectName("myapp"), None)

  /** Pre-condition guard for all tests in this suite. The toy processor is wired through bleep's dev-deps mechanism: tests reference it as
    * `build.bleep:bleep-test-ksp-processor:${BLEEP_VERSION}`, which `BleepDevDeps` resolves to its compiled `classes/` + `src/resources/` dirs in bleep's own
    * `.bleep/`. The bleep build itself must therefore have been compiled into the v2 layout (`.bleep/projects/<cross>/builds/normal/classes`). Until that
    * deploy cycle (sourcegen → compile → my-publish-local → re-deploy bleep CLI) has run on a branch carrying the layout-v2 change, the directory is missing
    * and these tests should skip rather than report a misleading failure.
    */
  private def requireToyProcessorBuilt(): Unit = {
    val expected = FileUtils.cwd.resolve(".bleep/projects/bleep-test-ksp-processor/builds/normal/classes")
    if (!Files.isDirectory(expected)) {
      cancel(
        s"bleep-test-ksp-processor not present at $expected. " +
          s"Run a fresh `bleep compile bleep-test-ksp-processor` from a layout-v2-aware bleep CLI to populate it."
      )
    }
  }

  integrationTest("@GenerateKotlin emits a sibling Kotlin class that user code can reference") { ws =>
    requireToyProcessorBuilt()
    ws.yaml(
      s"""projects:
         |  myapp:
         |    source-layout: kotlin
         |    kotlin:
         |      version: $kotlinVersion
         |      kspVersion: $kspVersion
         |      symbolProcessors:
         |        - $toy
         |    dependencies:
         |      - $toy
         |    platform:
         |      name: jvm
         |""".stripMargin
    )
    ws.file(
      "myapp/src/kotlin/myapp/Greeter.kt",
      """package myapp
        |
        |import bleep.test.ksp.GenerateKotlin
        |
        |@GenerateKotlin(suffix = "Helper")
        |class Greeter
        |
        |fun main() { println(GreeterHelper().originalName()) }
        |""".stripMargin
    )
    val (started, commands, _) = ws.start()
    commands.compile(List(projectName))

    val generated = started.buildPaths.generatedSourcesDir(projectName, "ksp").resolve("kotlin/myapp/GreeterHelper.kt")
    assert(Files.exists(generated), s"expected toy-processor output at $generated")
    succeed
  }

  integrationTest("@GenerateJava emits a .java that bleep mixed-compile picks up and Kotlin can call") { ws =>
    requireToyProcessorBuilt()
    ws.yaml(
      s"""projects:
         |  myapp:
         |    source-layout: kotlin
         |    kotlin:
         |      version: $kotlinVersion
         |      kspVersion: $kspVersion
         |      symbolProcessors:
         |        - $toy
         |    dependencies:
         |      - $toy
         |    platform:
         |      name: jvm
         |""".stripMargin
    )
    ws.file(
      "myapp/src/kotlin/myapp/Greeter.kt",
      """package myapp
        |
        |import bleep.test.ksp.GenerateJava
        |
        |@GenerateJava(suffix = "JavaHelper")
        |class Greeter
        |
        |fun main() { println(GreeterJavaHelper.originalName()) }
        |""".stripMargin
    )
    val (started, commands, _) = ws.start()
    commands.compile(List(projectName))

    val generatedSource = started.buildPaths.generatedSourcesDir(projectName, "ksp").resolve("java/myapp/GreeterJavaHelper.java")
    assert(Files.exists(generatedSource), s"expected generated Java source at $generatedSource")

    val classesDir = started.buildPaths.variantBuildDir(projectName).resolve("classes")
    val genClass = classesDir.resolve("myapp/GreeterJavaHelper.class")
    val kotlinClass = classesDir.resolve("myapp/GreeterKt.class")
    assert(Files.exists(genClass), s"expected javac to compile the KSP-emitted .java at $genClass")
    assert(Files.exists(kotlinClass), s"expected kotlinc to compile the calling Kotlin code at $kotlinClass")
    succeed
  }

  integrationTest("@ThrowOnMe surfaces as a compile failure") { ws =>
    requireToyProcessorBuilt()
    ws.yaml(
      s"""projects:
         |  myapp:
         |    source-layout: kotlin
         |    kotlin:
         |      version: $kotlinVersion
         |      kspVersion: $kspVersion
         |      symbolProcessors:
         |        - $toy
         |    dependencies:
         |      - $toy
         |    platform:
         |      name: jvm
         |""".stripMargin
    )
    ws.file(
      "myapp/src/kotlin/myapp/Boom.kt",
      """package myapp
        |
        |import bleep.test.ksp.ThrowOnMe
        |
        |@ThrowOnMe
        |class Boom
        |""".stripMargin
    )
    val (_, commands, _) = ws.start()
    val ex = intercept[Throwable](commands.compile(List(projectName)))
    assert(ex.getMessage != null && ex.getMessage.nonEmpty, "expected an error message from the failed compile")
    succeed
  }

  integrationTest("KSP configured on a Kotlin JS project fails fast with a clear platform message") { ws =>
    // No requireToyProcessorBuilt() needed — this test only validates plan-building, not actual KSP execution.
    ws.yaml(
      s"""projects:
         |  myapp:
         |    source-layout: kotlin
         |    kotlin:
         |      version: $kotlinVersion
         |      kspVersion: $kspVersion
         |      symbolProcessors:
         |        - com.squareup.moshi:moshi-kotlin-codegen:1.15.0
         |    platform:
         |      name: js
         |      jsVersion: 1.13.2
         |""".stripMargin
    )
    ws.file(
      "myapp/src/kotlin/myapp/Plain.kt",
      """package myapp
        |class Plain
        |""".stripMargin
    )
    val (_, commands, _) = ws.start()
    val ex = intercept[Throwable](commands.compile(List(projectName)))
    val msg = Option(ex.getMessage).getOrElse("") + Option(ex.getCause).map(c => " " + c.getMessage).getOrElse("")
    assert(
      msg.contains("KSP2") && msg.contains("JVM"),
      s"expected error to mention KSP2/JVM-only restriction, got: $msg"
    )
    succeed
  }

  integrationTest("processor-options reach the processor — toy processor writes an invocation log") { ws =>
    requireToyProcessorBuilt()
    // Symbol-processor options aren't run through the project's `${PROJECT_DIR}` template engine, so we
    // resolve the log path against the workspace root up-front and embed it as an absolute path. This also
    // makes the test independent of the KSP runner's working directory.
    val logPath = ws.root.resolve("ksp-invocations.log").toAbsolutePath
    ws.yaml(
      s"""projects:
         |  myapp:
         |    source-layout: kotlin
         |    kotlin:
         |      version: $kotlinVersion
         |      kspVersion: $kspVersion
         |      symbolProcessors:
         |        - $toy
         |      symbolProcessorOptions:
         |        bleep.test.ksp.log: "$logPath"
         |    dependencies:
         |      - $toy
         |    platform:
         |      name: jvm
         |""".stripMargin
    )
    ws.file(
      "myapp/src/kotlin/myapp/Plain.kt",
      """package myapp
        |
        |class Plain
        |""".stripMargin
    )
    val (_, commands, _) = ws.start()
    commands.compile(List(projectName))

    assert(Files.exists(logPath), s"expected toy-processor log at $logPath")
    val lines = Files.readAllLines(logPath)
    assert(lines.size >= 1, s"expected at least one invocation line after first compile, got: $lines")
    succeed
  }
}
