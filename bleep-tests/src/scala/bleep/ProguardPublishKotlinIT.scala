package bleep

/** Kotlin sibling of [[ProguardPublishJavaIT]]. Same publish flow (compile -> package -> ProGuard -> publishToLocalIvy), written as a Kotlin bleep script.
  * Demonstrates that the bleepscript Java API is fully usable from Kotlin.
  */
class ProguardPublishKotlinIT extends IntegrationTestHarness {

  integrationTest("proguard-publish-kotlin workspace compiles") { ws =>
    ws.yaml(
      snippet = "proguard-publish-kotlin/bleep.yaml",
      content = s"""projects:
                   |  mylib:
                   |    kotlin:
                   |      jvmTarget: "17"
                   |      version: ${model.VersionKotlin.Kotlin23.kotlinVersion}
                   |    platform:
                   |      name: jvm
                   |    publish:
                   |      groupId: com.example
                   |  scripts:
                   |    dependencies:
                   |      - build.bleep:bleepscript:$${BLEEP_VERSION}
                   |    kotlin:
                   |      jvmTarget: "${model.Jvm.graalvm.majorVersion}"
                   |      version: ${model.VersionKotlin.Kotlin23.kotlinVersion}
                   |    platform:
                   |      name: jvm
                   |scripts:
                   |  proguard-publish:
                   |    main: scripts.ProguardPublish
                   |    project: scripts
                   |""".stripMargin
    )

    ws.file(
      "mylib/src/kotlin/com/example/MyLib.kt",
      snippet = "proguard-publish-kotlin/MyLib.kt",
      content = """package com.example
                  |
                  |object MyLib {
                  |  fun greet(name: String): String = "Hello, $name!"
                  |}
                  |""".stripMargin
    )

    ws.file(
      "scripts/src/kotlin/scripts/Proguard.kt",
      snippet = "proguard-publish-kotlin/Proguard.kt",
      content = """package scripts
                  |
                  |import bleepscript.Cli
                  |import bleepscript.Coursier
                  |import bleepscript.Started
                  |import java.io.File
                  |import java.nio.file.Files
                  |import java.nio.file.Path
                  |import kotlin.io.path.deleteExisting
                  |
                  |/**
                  | * A reusable ProGuard helper. Takes JAR bytes, returns shrunk JAR bytes. Reusable
                  | * across projects: copy this file, depend on it via `sources:` in bleep.yaml, or
                  | * publish it as a library.
                  | *
                  | * This is what a "plugin" looks like when you don't have a plugin API: a regular
                  | * class with a regular method. No registration, no lifecycle, no autoplugin
                  | * trigger — just code you can call.
                  | *
                  | * Instantiate via [create] — the factory fetches ProGuard once and caches the
                  | * classpath. Reuse one instance across multiple shrink calls in the same script.
                  | */
                  |class Proguard private constructor(
                  |  private val started: Started,
                  |  private val proguardVersion: String,
                  |  private val classpath: String,
                  |) {
                  |
                  |  /**
                  |   * Run ProGuard over the given JAR bytes; return the shrunk JAR bytes. Writes
                  |   * input/output JARs to a temp directory and forks a JVM through bleep's cli
                  |   * wrapper.
                  |   */
                  |  fun shrink(inputJarBytes: ByteArray): ByteArray {
                  |    val workdir: Path = Files.createTempDirectory("proguard")
                  |    try {
                  |      val inputJar = workdir.resolve("input.jar")
                  |      val outputJar = workdir.resolve("output.jar")
                  |      val configFile = workdir.resolve("config.pro")
                  |      Files.write(inputJar, inputJarBytes)
                  |      Files.writeString(configFile, config(inputJar, outputJar))
                  |
                  |      started.logger().info("running ProGuard $proguardVersion")
                  |      Cli.command("proguard")
                  |        .args(
                  |          started.jvmCommand().toString(),
                  |          "-cp", classpath,
                  |          "proguard.ProGuard",
                  |          "@${configFile.toAbsolutePath()}"
                  |        )
                  |        .cwd(workdir)
                  |        .run(started)
                  |
                  |      return Files.readAllBytes(outputJar)
                  |    } finally {
                  |      Files.walk(workdir).use { stream ->
                  |        stream.sorted(Comparator.reverseOrder()).forEach { it.deleteExisting() }
                  |      }
                  |    }
                  |  }
                  |
                  |  /**
                  |   * Minimal ProGuard config: shrink only (no obfuscation or optimization), keep
                  |   * all public members so the published artifact stays byte-compatible. Customize
                  |   * for real use.
                  |   */
                  |  private fun config(inputJar: Path, outputJar: Path): String =
                  |    listOf(
                  |      "-injars ${inputJar.toAbsolutePath()}",
                  |      "-outjars ${outputJar.toAbsolutePath()}",
                  |      "-dontoptimize",
                  |      "-dontobfuscate",
                  |      "-dontwarn",
                  |      "-keep class ** { *; }"
                  |    ).joinToString("\n")
                  |
                  |  companion object {
                  |    const val DEFAULT_VERSION = "7.7.0"
                  |
                  |    /** Resolve and cache the ProGuard classpath. */
                  |    fun create(started: Started, proguardVersion: String = DEFAULT_VERSION): Proguard {
                  |      val jars = Coursier.fetchClasspath(
                  |        started, "com.guardsquare:proguard-base:$proguardVersion"
                  |      )
                  |      val classpath = jars.joinToString(File.pathSeparator) { it.toString() }
                  |      return Proguard(started, proguardVersion, classpath)
                  |    }
                  |  }
                  |}
                  |""".stripMargin
    )

    ws.file(
      "scripts/src/kotlin/scripts/ProguardPublish.kt",
      snippet = "proguard-publish-kotlin/ProguardPublish.kt",
      content = """package scripts
                  |
                  |import bleepscript.BleepScript
                  |import bleepscript.BleepscriptServices
                  |import bleepscript.Commands
                  |import bleepscript.CrossProjectName
                  |import bleepscript.PackagedLibrary
                  |import bleepscript.Packaging
                  |import bleepscript.PublishLayout
                  |import bleepscript.Started
                  |import java.util.Optional
                  |
                  |/**
                  | * Compose: compile, package, [Proguard.shrink] the main JAR, publish.
                  | *
                  | * The "plugin" lives next door in [Proguard]. This script is just glue: arg
                  | * parsing + four bleepscript API calls.
                  | *
                  | * Usage: `bleep proguard-publish <projectName> <version> <groupId>`.
                  | */
                  |class ProguardPublish : BleepScript("proguard-publish") {
                  |  override fun run(started: Started, commands: Commands, args: List<String>) {
                  |    require(args.size >= 3) {
                  |      "Usage: bleep proguard-publish <projectName> <version> <groupId>"
                  |    }
                  |    val projectName = args[0]
                  |    val version = args[1]
                  |    val groupId = args[2]
                  |
                  |    val project = CrossProjectName(projectName, Optional.empty())
                  |    val log = started.logger().withContext("project", projectName)
                  |
                  |    commands.compile(listOf(project))
                  |
                  |    log.info("packaging")
                  |    val library = Packaging.packageProject(
                  |      started,
                  |      project,
                  |      groupId,
                  |      version,
                  |      PublishLayout.Maven.INSTANCE,
                  |      BleepscriptServices.Holder.INSTANCE.defaultManifestCreator()
                  |    )
                  |
                  |    val originalJar = library.jarFile()
                  |    log.info("main JAR: ${library.jarFilePath().asString()} (${originalJar.size} bytes)")
                  |
                  |    val shrunk = Proguard.create(started).shrink(originalJar)
                  |    log.info("ProGuard output: ${shrunk.size} bytes (saved ${originalJar.size - shrunk.size})")
                  |
                  |    Packaging.publishToLocalMaven(library.withJarFile(shrunk))
                  |    log.info("Published $groupId:$projectName:$version")
                  |  }
                  |}
                  |""".stripMargin
    )

    ws.compileAll()
    succeed
  }
}
