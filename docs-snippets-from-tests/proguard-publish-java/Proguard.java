package scripts;

import bleepscript.Cli;
import bleepscript.Coursier;
import bleepscript.Started;
import java.io.File;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * A reusable ProGuard helper. Takes JAR bytes, returns shrunk JAR bytes. Reusable
 * across projects: copy this file, depend on it via {@code sources:} in
 * bleep.yaml, or publish it as a library.
 *
 * <p>This is what a "plugin" looks like when you don't have a plugin API: a
 * regular class with a regular method. No registration, no lifecycle, no
 * autoplugin trigger — just code you can call.
 *
 * <p>Instantiate via {@link #create(Started)} — the factory fetches ProGuard
 * once and caches the classpath. Reuse one instance across multiple shrink
 * calls in the same script.
 */
public record Proguard(Started started, String proguardVersion, String classpath) {
  public static final String DEFAULT_VERSION = "7.7.0";

  /** Resolve and cache the ProGuard classpath. Default version. */
  public static Proguard create(Started started) {
    return create(started, DEFAULT_VERSION);
  }

  /** Resolve and cache the ProGuard classpath. Caller picks the version. */
  public static Proguard create(Started started, String proguardVersion) {
    List<Path> jars =
        Coursier.fetchClasspath(
            started, "com.guardsquare:proguard-base:" + proguardVersion);
    String classpath =
        jars.stream().map(Path::toString).collect(Collectors.joining(File.pathSeparator));
    return new Proguard(started, proguardVersion, classpath);
  }

  /**
   * Run ProGuard over the given JAR bytes; return the shrunk JAR bytes. Writes
   * input/output JARs to a temp directory and forks a JVM through bleep's cli
   * wrapper using the JVM bleep resolved for this build.
   */
  public byte[] shrink(byte[] inputJarBytes) {
    try {
      Path workdir = Files.createTempDirectory("proguard");
      try {
        Path inputJar = workdir.resolve("input.jar");
        Path outputJar = workdir.resolve("output.jar");
        Path configFile = workdir.resolve("config.pro");
        Files.write(inputJar, inputJarBytes);
        Files.writeString(configFile, config(inputJar, outputJar));

        started.logger().info("running ProGuard " + proguardVersion);
        Cli.command("proguard")
            .args(
                started.jvmCommand().toString(),
                "-cp",
                classpath,
                "proguard.ProGuard",
                "@" + configFile.toAbsolutePath())
            .cwd(workdir)
            .run(started);

        return Files.readAllBytes(outputJar);
      } finally {
        deleteRecursively(workdir);
      }
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  private static void deleteRecursively(Path dir) throws IOException {
    try (Stream<Path> paths = Files.walk(dir)) {
      for (Path p : paths.sorted(Comparator.reverseOrder()).toList()) {
        Files.delete(p);
      }
    }
  }

  /**
   * Minimal ProGuard config: shrink only (no obfuscation or optimization), keep
   * all public members so the published artifact stays byte-compatible. Customize
   * for real use.
   */
  private static String config(Path inputJar, Path outputJar) {
    return String.join(
        "\n",
        "-injars " + inputJar.toAbsolutePath(),
        "-outjars " + outputJar.toAbsolutePath(),
        "-dontoptimize",
        "-dontobfuscate",
        "-dontwarn",
        "-keep class ** { *; }");
  }
}
