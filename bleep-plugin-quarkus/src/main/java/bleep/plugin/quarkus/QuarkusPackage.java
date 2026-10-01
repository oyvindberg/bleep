package bleep.plugin.quarkus;

import bleepscript.BleepScript;
import bleepscript.Commands;
import bleepscript.CrossProjectName;
import bleepscript.Started;
import java.io.File;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Production build for a Quarkus application. Equivalent to {@code mvn package} on a Quarkus
 * project: runs the augmentor's {@code createProductionApplication()} off bleep's application
 * model, writing the packaged output (fast-jar {@code quarkus-app/} by default) and {@code
 * quarkus-artifact.properties} — which is also exactly what {@code @QuarkusIntegrationTest}
 * launches from (point it there with {@code -Dbuild.output.directory}).
 *
 * <p>Output lands in {@code <project target dir>/quarkus-build} unless overridden with {@link
 * #withOutputDir}. Quarkus packaging config applies as usual ({@code quarkus.package.jar.type} in
 * {@code application.properties} for uber-jar, etc.).
 */
public class QuarkusPackage extends BleepScript {
  private final List<String> jvmArgs = new ArrayList<>();
  private Path outputDir;

  public QuarkusPackage() {
    super("quarkus-package");
  }

  /** Extra JVM arguments for the augmentation JVM. */
  public QuarkusPackage withJvmArgs(String... args) {
    Collections.addAll(this.jvmArgs, args);
    return this;
  }

  /**
   * Where the packaged application is written. Default: {@code <project target dir>/quarkus-build}.
   */
  public QuarkusPackage withOutputDir(Path outputDir) {
    this.outputDir = outputDir;
    return this;
  }

  @Override
  public void run(Started started, Commands commands, List<String> args) {
    if (args.size() != 1) {
      throw new IllegalArgumentException(
          "quarkus-package requires exactly one argument: the project name");
    }
    packageOn(started, commands, args.get(0));
  }

  /**
   * Package the named Quarkus application project. Compiles first, builds (or reuses) the prod
   * application model, then forks {@code bleep.quarkus.QuarkusPackager} with the app's own Quarkus
   * version on the classpath.
   *
   * @return the directory the packaged application was written to
   */
  public Path packageOn(Started started, Commands commands, String projectName) {
    CrossProjectName cross = CrossProjectName.of(projectName);
    commands.compile(List.of(cross));

    Path model = QuarkusAppModel.ensureAppModel(started, cross, "prod");
    List<Path> runnerClasspath = QuarkusAppModel.runnerClasspath(started, cross);
    Path out =
        outputDir != null
            ? outputDir
            : started.projectPaths(cross).targetDir().resolve("quarkus-build");

    List<String> command = new ArrayList<>();
    command.add(started.jvmCommand().toString());
    command.addAll(jvmArgs);
    command.add("-cp");
    command.add(
        runnerClasspath.stream()
            .map(Path::toString)
            .collect(Collectors.joining(File.pathSeparator)));
    command.add("bleep.quarkus.QuarkusPackager");
    command.add(model.toString());
    command.add(out.toString());

    started.logger().info("Packaging " + projectName + " with Quarkus into " + out);

    ProcessBuilder pb = new ProcessBuilder(command);
    pb.inheritIO();
    try {
      Process p = pb.start();
      int exit = p.waitFor();
      if (exit != 0) {
        throw new RuntimeException(
            "Quarkus packaging of " + projectName + " failed with exit code " + exit);
      }
      return out;
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      throw new RuntimeException("Interrupted while packaging " + projectName, e);
    } catch (Exception e) {
      throw new RuntimeException("Failed to package " + projectName, e);
    }
  }
}
