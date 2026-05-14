package scripts

import bleepscript.Cli
import bleepscript.Coursier
import bleepscript.Started
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import kotlin.io.path.deleteExisting

/**
 * A reusable ProGuard helper. Takes JAR bytes, returns shrunk JAR bytes. Reusable
 * across projects: copy this file, depend on it via `sources:` in bleep.yaml, or
 * publish it as a library.
 *
 * This is what a "plugin" looks like when you don't have a plugin API: a regular
 * class with a regular method. No registration, no lifecycle, no autoplugin
 * trigger — just code you can call.
 *
 * Instantiate via [create] — the factory fetches ProGuard once and caches the
 * classpath. Reuse one instance across multiple shrink calls in the same script.
 */
class Proguard private constructor(
  private val started: Started,
  private val proguardVersion: String,
  private val classpath: String,
) {

  /**
   * Run ProGuard over the given JAR bytes; return the shrunk JAR bytes. Writes
   * input/output JARs to a temp directory and forks a JVM through bleep's cli
   * wrapper.
   */
  fun shrink(inputJarBytes: ByteArray): ByteArray {
    val workdir: Path = Files.createTempDirectory("proguard")
    try {
      val inputJar = workdir.resolve("input.jar")
      val outputJar = workdir.resolve("output.jar")
      val configFile = workdir.resolve("config.pro")
      Files.write(inputJar, inputJarBytes)
      Files.writeString(configFile, config(inputJar, outputJar))

      started.logger().info("running ProGuard $proguardVersion")
      Cli.command("proguard")
        .args(
          started.jvmCommand().toString(),
          "-cp", classpath,
          "proguard.ProGuard",
          "@${configFile.toAbsolutePath()}"
        )
        .cwd(workdir)
        .run(started)

      return Files.readAllBytes(outputJar)
    } finally {
      Files.walk(workdir).use { stream ->
        stream.sorted(Comparator.reverseOrder()).forEach { it.deleteExisting() }
      }
    }
  }

  /**
   * Minimal ProGuard config: shrink only (no obfuscation or optimization), keep
   * all public members so the published artifact stays byte-compatible. Customize
   * for real use.
   */
  private fun config(inputJar: Path, outputJar: Path): String =
    listOf(
      "-injars ${inputJar.toAbsolutePath()}",
      "-outjars ${outputJar.toAbsolutePath()}",
      "-dontoptimize",
      "-dontobfuscate",
      "-dontwarn",
      "-keep class ** { *; }"
    ).joinToString("\n")

  companion object {
    const val DEFAULT_VERSION = "7.7.0"

    /** Resolve and cache the ProGuard classpath. */
    fun create(started: Started, proguardVersion: String = DEFAULT_VERSION): Proguard {
      val jars = Coursier.fetchClasspath(
        started, "com.guardsquare:proguard-base:$proguardVersion"
      )
      val classpath = jars.joinToString(File.pathSeparator) { it.toString() }
      return Proguard(started, proguardVersion, classpath)
    }
  }
}
