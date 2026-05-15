package bleep

import java.nio.file.Files

/** End-to-end KSP1 integration tests. Exercises the whole stack: model → resolver → DAG handler → kotlinc plugin classpath → real KSP processor → generated
  * source files on disk → downstream compile picking them up.
  *
  * Uses Moshi codegen as the canonical real-world KSP processor. JVM-pure, available on Maven Central, generates a `*JsonAdapter` for each
  * `@JsonClass(generateAdapter = true)` Kotlin class.
  *
  * Kotlin/KSP version pair: 2.1.20 + 1.0.32 (KSP releases are pinned 1:1 to kotlinc versions; see https://github.com/google/ksp/releases).
  */
class KspBasicIT extends IntegrationTestHarness {

  private val kotlinVersion = "2.1.20"
  private val kspVersion = "1.0.32"
  private val moshi = "com.squareup.moshi:moshi:1.15.0"
  private val moshiCodegen = "com.squareup.moshi:moshi-kotlin-codegen:1.15.0"

  integrationTest("kspResolves — real KSP processor (Moshi codegen) runs and emits a JsonAdapter") { ws =>
    ws.yaml(
      s"""projects:
         |  myapp:
         |    source-layout: kotlin
         |    kotlin:
         |      version: $kotlinVersion
         |      kspVersion: $kspVersion
         |      symbolProcessors:
         |        - $moshiCodegen
         |    dependencies:
         |      - $moshi
         |    platform:
         |      name: jvm
         |""".stripMargin
    )
    ws.file(
      "myapp/src/kotlin/myapp/Person.kt",
      """package myapp
        |
        |import com.squareup.moshi.JsonClass
        |
        |@JsonClass(generateAdapter = true)
        |data class Person(val name: String, val age: Int)
        |""".stripMargin
    )
    val (started, commands, _) = ws.start()
    commands.compile(List(model.CrossProjectName(model.ProjectName("myapp"), None)))

    val crossName = model.CrossProjectName(model.ProjectName("myapp"), None)
    val kspKotlinDir = started.buildPaths.generatedSourcesDir(crossName, "ksp").resolve("kotlin")
    val generated = if (Files.exists(kspKotlinDir)) {
      Files.walk(kspKotlinDir).toArray.map(_.toString).filter(_.endsWith(".kt")).toList
    } else Nil
    assert(
      generated.exists(_.endsWith("PersonJsonAdapter.kt")),
      s"expected PersonJsonAdapter.kt under $kspKotlinDir, got: ${generated.mkString(", ")}"
    )
    succeed
  }

  integrationTest("kspMissingKspVersion — symbolProcessors without kspVersion fails loud") { ws =>
    ws.yaml(
      s"""projects:
         |  myapp:
         |    source-layout: kotlin
         |    kotlin:
         |      version: $kotlinVersion
         |      symbolProcessors:
         |        - $moshiCodegen
         |    dependencies:
         |      - $moshi
         |    platform:
         |      name: jvm
         |""".stripMargin
    )
    ws.file(
      "myapp/src/kotlin/myapp/Trivial.kt",
      """package myapp
        |
        |class Trivial
        |""".stripMargin
    )
    val (_, commands, storingLogger) = ws.start()
    val ex = intercept[Throwable](commands.compile(List(model.CrossProjectName(model.ProjectName("myapp"), None))))
    val msg = Option(ex.getMessage).getOrElse("") + Option(ex.getCause).map(_.getMessage).getOrElse("")
    assert(msg.contains("KSP processor resolution failed"), s"unexpected summary message: $msg")
    // The specific reason is logged.
    val logged = storingLogger.underlying.map(_.message.plainText).mkString("\n")
    assert(
      logged.contains("kspVersion"),
      s"expected log to mention kspVersion in detail; got:\n$logged"
    )
  }

  integrationTest("kspBadCoord — nonexistent kspVersion fails with coursier error") { ws =>
    ws.yaml(
      s"""projects:
         |  myapp:
         |    source-layout: kotlin
         |    kotlin:
         |      version: $kotlinVersion
         |      kspVersion: 99.0.0
         |      symbolProcessors:
         |        - $moshiCodegen
         |    dependencies:
         |      - $moshi
         |    platform:
         |      name: jvm
         |""".stripMargin
    )
    ws.file(
      "myapp/src/kotlin/myapp/Trivial.kt",
      """package myapp
        |
        |class Trivial
        |""".stripMargin
    )
    val (_, commands, storingLogger) = ws.start()
    val ex = intercept[Throwable](commands.compile(List(model.CrossProjectName(model.ProjectName("myapp"), None))))
    val msg = Option(ex.getMessage).getOrElse("") + Option(ex.getCause).map(_.getMessage).getOrElse("")
    assert(msg.contains("KSP processor resolution failed"), s"unexpected summary message: $msg")
    val logged = storingLogger.underlying.map(_.message.plainText).mkString("\n")
    assert(
      logged.contains("symbol-processing") || logged.contains(s"$kotlinVersion-99.0.0") || logged.contains("99.0.0"),
      s"expected log to mention the full unresolved KSP coord; got:\n$logged"
    )
  }

  integrationTest("kspNoProcessorJars — scanForSymbolProcessors=true with no processor jars fails loud") { ws =>
    ws.yaml(
      s"""projects:
         |  myapp:
         |    source-layout: kotlin
         |    kotlin:
         |      version: $kotlinVersion
         |      kspVersion: $kspVersion
         |      scanForSymbolProcessors: true
         |    dependencies:
         |      - $moshi
         |    platform:
         |      name: jvm
         |""".stripMargin
    )
    ws.file(
      "myapp/src/kotlin/myapp/Trivial.kt",
      """package myapp
        |
        |class Trivial
        |""".stripMargin
    )
    val (_, commands, storingLogger) = ws.start()
    val ex = intercept[Throwable](commands.compile(List(model.CrossProjectName(model.ProjectName("myapp"), None))))
    val msg = Option(ex.getMessage).getOrElse("") + Option(ex.getCause).map(_.getMessage).getOrElse("")
    assert(msg.contains("KSP processor resolution failed"), s"unexpected summary message: $msg")
    val logged = storingLogger.underlying.map(_.message.plainText).mkString("\n")
    assert(
      logged.contains("scanForSymbolProcessors") || logged.contains("no KSP processor JARs"),
      s"expected log to mention empty scan opt-in; got:\n$logged"
    )
  }

  integrationTest("kspScanFindsProcessor — scanForSymbolProcessors=true picks up Moshi codegen on the classpath") { ws =>
    ws.yaml(
      s"""projects:
         |  myapp:
         |    source-layout: kotlin
         |    kotlin:
         |      version: $kotlinVersion
         |      kspVersion: $kspVersion
         |      scanForSymbolProcessors: true
         |    dependencies:
         |      - $moshi
         |      - $moshiCodegen
         |    platform:
         |      name: jvm
         |""".stripMargin
    )
    ws.file(
      "myapp/src/kotlin/myapp/Animal.kt",
      """package myapp
        |
        |import com.squareup.moshi.JsonClass
        |
        |@JsonClass(generateAdapter = true)
        |data class Animal(val species: String, val legs: Int)
        |""".stripMargin
    )
    val (started, commands, _) = ws.start()
    commands.compile(List(model.CrossProjectName(model.ProjectName("myapp"), None)))

    val crossName = model.CrossProjectName(model.ProjectName("myapp"), None)
    val kspKotlinDir = started.buildPaths.generatedSourcesDir(crossName, "ksp").resolve("kotlin")
    val generated = if (Files.exists(kspKotlinDir)) {
      Files.walk(kspKotlinDir).toArray.map(_.toString).filter(_.endsWith(".kt")).toList
    } else Nil
    assert(
      generated.exists(_.endsWith("AnimalJsonAdapter.kt")),
      s"expected AnimalJsonAdapter.kt under $kspKotlinDir, got: ${generated.mkString(", ")}"
    )
    succeed
  }
}
