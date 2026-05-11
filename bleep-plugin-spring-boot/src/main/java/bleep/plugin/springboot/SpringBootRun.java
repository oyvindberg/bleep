package bleep.plugin.springboot;

import bleepscript.BleepScript;
import bleepscript.Commands;
import bleepscript.CrossProjectName;
import bleepscript.PlatformConfig;
import bleepscript.Project;
import bleepscript.ResolvedProject;
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
 * Forks a JVM to run a Spring Boot application against the project's compiled classes plus its
 * runtime classpath. Equivalent to {@code mvn spring-boot:run}.
 *
 * <p>Two usage modes:
 *
 * <ol>
 *   <li><b>Direct.</b> Reference {@code bleep.plugin.springboot.SpringBootRun} as the {@code main}
 *       of a {@code scripts:} entry. Invoke with {@code bleep scripts <script-name> <project>}. All
 *       flags use their defaults.
 *   <li><b>Wrapped.</b> Write your own {@link BleepScript} that instantiates this class, calls
 *       fluent setters to lock in the JVM args, profiles, system properties, and agents you want,
 *       and ends with {@link #runOn}. This is the recommended pattern for projects with multiple
 *       run-modes (dev, prod, staging) — one script per combination, branching in plain Java.
 * </ol>
 *
 * <p>DevTools and LiveReload require no special integration: if {@code spring-boot-devtools} is on
 * the runtime classpath, Spring Boot's startup wires it up itself.
 */
public class SpringBootRun extends BleepScript {
  private final List<String> jvmArgs = new ArrayList<>();
  private final List<String> profiles = new ArrayList<>();
  private final List<Path> agents = new ArrayList<>();
  private final Map<String, String> systemProperties = new LinkedHashMap<>();
  private final Map<String, String> environment = new LinkedHashMap<>();
  private final List<String> appArgs = new ArrayList<>();
  private boolean addResources = true;
  private boolean optimizedLaunch = true;
  private String mainClassOverride;
  private Path workingDirectory;

  public SpringBootRun() {
    super("springboot-run");
  }

  /** Extra JVM arguments (e.g. {@code "-Xmx512m"}). */
  public SpringBootRun withJvmArgs(String... args) {
    Collections.addAll(this.jvmArgs, args);
    return this;
  }

  /** Spring profiles to activate. Joined with commas into {@code --spring.profiles.active=...}. */
  public SpringBootRun withProfiles(String... profiles) {
    Collections.addAll(this.profiles, profiles);
    return this;
  }

  /** Add {@code src/main/resources} directories directly to the classpath for live edits. */
  public SpringBootRun withAddResources(boolean addResources) {
    this.addResources = addResources;
    return this;
  }

  /** Add {@code -XX:TieredStopAtLevel=1} for faster dev-mode startup. Default true. */
  public SpringBootRun withOptimizedLaunch(boolean optimizedLaunch) {
    this.optimizedLaunch = optimizedLaunch;
    return this;
  }

  /** Java agents to attach via {@code -javaagent:}. */
  public SpringBootRun withAgents(Path... agents) {
    Collections.addAll(this.agents, agents);
    return this;
  }

  /** System property to set on the forked JVM. */
  public SpringBootRun withSystemProperty(String key, String value) {
    this.systemProperties.put(key, value);
    return this;
  }

  /** Environment variable to set on the forked JVM. */
  public SpringBootRun withEnvironment(String key, String value) {
    this.environment.put(key, value);
    return this;
  }

  /** Override the main class auto-detected from the project's {@code platform.mainClass}. */
  public SpringBootRun withMainClass(String mainClass) {
    this.mainClassOverride = mainClass;
    return this;
  }

  /** Working directory for the forked JVM. Default: workspace root. */
  public SpringBootRun withWorkingDirectory(Path workingDirectory) {
    this.workingDirectory = workingDirectory;
    return this;
  }

  /** Application arguments (after the main class). */
  public SpringBootRun withAppArgs(String... args) {
    Collections.addAll(this.appArgs, args);
    return this;
  }

  /**
   * BleepScript entry point. Reads the project name from {@code args[0]} and forwards everything
   * else to the application.
   */
  @Override
  public void run(Started started, Commands commands, List<String> args) {
    if (args.isEmpty()) {
      throw new IllegalArgumentException(
          "springboot-run requires a project name as the first argument");
    }
    withAppArgs(args.subList(1, args.size()).toArray(new String[0]));
    int exit = runOn(started, commands, args.get(0));
    if (exit != 0) {
      throw new RuntimeException("Application exited with code " + exit);
    }
  }

  /**
   * Run the named project. Compiles first, then forks a JVM with the resolved classpath, the
   * configured JVM args, profile flag, agents, system properties, and environment.
   *
   * @return the JVM exit code
   */
  public int runOn(Started started, Commands commands, String projectName) {
    CrossProjectName cross = CrossProjectName.of(projectName);
    commands.compile(List.of(cross));

    ResolvedProject resolved = started.resolved(cross);
    Project exploded = started.exploded(cross);
    String mainClass = resolveMainClass(exploded, projectName);

    List<Path> classpath = new ArrayList<>();
    classpath.add(resolved.classesDir());
    if (addResources) {
      resolved.resources().ifPresent(classpath::addAll);
    }
    classpath.addAll(resolved.classpath());

    List<String> command = new ArrayList<>();
    command.add(started.jvmCommand().toString());
    if (optimizedLaunch) {
      command.add("-XX:TieredStopAtLevel=1");
    }
    for (Path agent : agents) {
      command.add("-javaagent:" + agent);
    }
    systemProperties.forEach((k, v) -> command.add("-D" + k + "=" + v));
    command.addAll(jvmArgs);
    command.add("-cp");
    command.add(
        classpath.stream().map(Path::toString).collect(Collectors.joining(File.pathSeparator)));
    command.add(mainClass);
    if (!profiles.isEmpty()) {
      command.add("--spring.profiles.active=" + String.join(",", profiles));
    }
    command.addAll(appArgs);

    started.logger().info("Launching " + projectName + " main " + mainClass);

    ProcessBuilder pb = new ProcessBuilder(command);
    if (workingDirectory != null) {
      pb.directory(workingDirectory.toFile());
    }
    pb.environment().putAll(environment);
    pb.inheritIO();

    try {
      Process p = pb.start();
      Runtime.getRuntime().addShutdownHook(new Thread(p::destroy, "springboot-run-shutdown"));
      return p.waitFor();
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      throw new RuntimeException("Interrupted while running " + projectName, e);
    } catch (Exception e) {
      throw new RuntimeException("Failed to run " + projectName, e);
    }
  }

  private String resolveMainClass(Project exploded, String projectName) {
    if (mainClassOverride != null) {
      return mainClassOverride;
    }
    return exploded
        .platform()
        .flatMap(PlatformConfig::mainClass)
        .orElseThrow(
            () ->
                new IllegalStateException(
                    "Project "
                        + projectName
                        + " has no platform.mainClass set in bleep.yaml; "
                        + "set it, or call .withMainClass(...) on SpringBootRun."));
  }
}
