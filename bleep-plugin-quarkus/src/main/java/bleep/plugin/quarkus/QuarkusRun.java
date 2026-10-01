package bleep.plugin.quarkus;

import bleepscript.BleepScript;
import bleepscript.Commands;
import bleepscript.CrossProjectName;
import bleepscript.Started;
import java.io.File;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Runs a Quarkus application in dev mode. Equivalent to {@code mvn quarkus:dev}: augmentation runs
 * in-process off bleep's application model, and Quarkus's own dev-mode machinery watches the source
 * directories recorded in that model and recompiles on change — live reload included, no build tool
 * in the loop.
 *
 * <p>Two usage modes, mirroring {@code bleep.plugin.springboot.SpringBootRun}:
 *
 * <ol>
 *   <li><b>Direct.</b> Reference {@code bleep.plugin.quarkus.QuarkusRun} as the {@code main} of a
 *       {@code scripts:} entry and invoke with {@code bleep scripts <script-name> <project>
 *       [app-args...]}.
 *   <li><b>Wrapped.</b> Instantiate from your own {@link BleepScript}, call the fluent setters, and
 *       end with {@link #runOn}.
 * </ol>
 */
public class QuarkusRun extends BleepScript {
  private final List<String> jvmArgs = new ArrayList<>();
  private final Map<String, String> systemProperties = new LinkedHashMap<>();
  private final Map<String, String> environment = new LinkedHashMap<>();
  private final List<String> appArgs = new ArrayList<>();
  private Path workingDirectory;

  public QuarkusRun() {
    super("quarkus-run");
  }

  /** Extra JVM arguments for the application JVM (e.g. {@code "-Xmx512m"}). */
  public QuarkusRun withJvmArgs(String... args) {
    Collections.addAll(this.jvmArgs, args);
    return this;
  }

  /** System property to set on the application JVM. */
  public QuarkusRun withSystemProperty(String key, String value) {
    this.systemProperties.put(key, value);
    return this;
  }

  /** Environment variable to set on the application JVM. */
  public QuarkusRun withEnvironment(String key, String value) {
    this.environment.put(key, value);
    return this;
  }

  /** Working directory for the application JVM. Default: workspace root. */
  public QuarkusRun withWorkingDirectory(Path workingDirectory) {
    this.workingDirectory = workingDirectory;
    return this;
  }

  /** Application arguments. */
  public QuarkusRun withAppArgs(String... args) {
    Collections.addAll(this.appArgs, args);
    return this;
  }

  @Override
  public void run(Started started, Commands commands, List<String> args) {
    if (args.isEmpty()) {
      throw new IllegalArgumentException(
          "quarkus-run requires a project name as the first argument");
    }
    withAppArgs(args.subList(1, args.size()).toArray(new String[0]));
    int exit = runOn(started, commands, args.get(0));
    if (exit != 0) {
      throw new RuntimeException("Application exited with code " + exit);
    }
  }

  /**
   * Run the named Quarkus application project in dev mode. Compiles first, builds (or reuses) the
   * application model, then forks {@code bleep.quarkus.QuarkusDevRun} with the app's own Quarkus
   * version on the classpath.
   *
   * @return the JVM exit code
   */
  public int runOn(Started started, Commands commands, String projectName) {
    CrossProjectName cross = CrossProjectName.of(projectName);
    commands.compile(List.of(cross));

    Path model = QuarkusAppModel.ensureAppModel(started, cross, "dev");
    List<Path> runnerClasspath = QuarkusAppModel.runnerClasspath(started, cross);

    List<String> command = new ArrayList<>();
    command.add(started.jvmCommand().toString());
    systemProperties.forEach((k, v) -> command.add("-D" + k + "=" + v));
    command.addAll(jvmArgs);
    command.add("-cp");
    command.add(
        runnerClasspath.stream()
            .map(Path::toString)
            .collect(Collectors.joining(File.pathSeparator)));
    command.add("bleep.quarkus.QuarkusDevRun");
    command.add(model.toString());
    command.addAll(appArgs);

    started.logger().info("Launching " + projectName + " in Quarkus dev mode");

    ProcessBuilder pb = new ProcessBuilder(command);
    if (workingDirectory != null) {
      pb.directory(workingDirectory.toFile());
    }
    pb.environment().putAll(environment);
    pb.inheritIO();

    try {
      Process p = pb.start();
      Runtime.getRuntime().addShutdownHook(new Thread(p::destroy, "quarkus-run-shutdown"));
      return p.waitFor();
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      throw new RuntimeException("Interrupted while running " + projectName, e);
    } catch (Exception e) {
      throw new RuntimeException("Failed to run " + projectName, e);
    }
  }
}
