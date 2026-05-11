package bleep.plugin.springboot;

import bleepscript.BleepCodegenScript;
import bleepscript.CodegenTarget;
import bleepscript.Commands;
import bleepscript.Project;
import bleepscript.PublishConfig;
import bleepscript.Started;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.springframework.boot.loader.tools.BuildPropertiesWriter;
import org.springframework.boot.loader.tools.BuildPropertiesWriter.ProjectDetails;

/**
 * Writes {@code META-INF/build-info.properties} for one or more projects. Read at runtime by Spring
 * Boot Actuator's {@code /actuator/info} endpoint.
 *
 * <p>Usage as a sourcegen entry in {@code bleep.yaml}:
 *
 * <pre>{@code
 * projects:
 *   myapp:
 *     sourcegen:
 *       - main: bleep.plugin.springboot.SpringBootBuildInfo
 *         project: scripts
 * }</pre>
 *
 * <p>For per-project version or additional properties, write your own {@link BleepCodegenScript}
 * that instantiates this class, configures it fluently, and calls {@link #writeFor} per target.
 */
public class SpringBootBuildInfo extends BleepCodegenScript {
  private String version;
  private String name;
  private String groupOverride;
  private Instant timestamp;
  private boolean includeTime = true;
  private final Map<String, String> additional = new LinkedHashMap<>();

  public SpringBootBuildInfo() {
    super("springboot-build-info");
  }

  /** Set the version written to build-info. Default: empty string. */
  public SpringBootBuildInfo withVersion(String version) {
    this.version = version;
    return this;
  }

  /** Override the project name. Default: the project's bleep name. */
  public SpringBootBuildInfo withName(String name) {
    this.name = name;
    return this;
  }

  /** Override the groupId. Default: from the project's publish config, otherwise empty. */
  public SpringBootBuildInfo withGroupId(String groupId) {
    this.groupOverride = groupId;
    return this;
  }

  /** Override the build timestamp. Default: {@link Instant#now()}. */
  public SpringBootBuildInfo withTimestamp(Instant timestamp) {
    this.timestamp = timestamp;
    return this;
  }

  /** Whether to write the {@code build.time} property. Default true. */
  public SpringBootBuildInfo withIncludeTime(boolean includeTime) {
    this.includeTime = includeTime;
    return this;
  }

  /** Extra {@code build.<key>} property. */
  public SpringBootBuildInfo withAdditional(String key, String value) {
    this.additional.put(key, value);
    return this;
  }

  @Override
  public void run(
      Started started, Commands commands, List<CodegenTarget> targets, List<String> args) {
    for (CodegenTarget target : targets) {
      writeFor(started, target);
    }
  }

  /** Write build-info.properties for a single codegen target. */
  public void writeFor(Started started, CodegenTarget target) {
    Project project = started.exploded(target.project());
    String resolvedGroup =
        groupOverride != null
            ? groupOverride
            : project.publish().flatMap(PublishConfig::groupId).orElse("");
    String resolvedName = name != null ? name : target.project().name();
    String resolvedVersion = version != null ? version : "";
    Instant resolvedTime = includeTime ? (timestamp != null ? timestamp : Instant.now()) : null;

    ProjectDetails details =
        new ProjectDetails(
            resolvedGroup, resolvedName, resolvedVersion, resolvedName, resolvedTime, additional);

    Path output = target.resources().resolve("META-INF/build-info.properties");
    try {
      Files.createDirectories(output.getParent());
      new BuildPropertiesWriter(output.toFile()).writeBuildProperties(details);
    } catch (IOException e) {
      throw new RuntimeException("Failed to write " + output, e);
    } catch (BuildPropertiesWriter.NullAdditionalPropertyValueException e) {
      throw new RuntimeException("Null value in additional properties", e);
    }
    started.logger().info("Wrote " + output);
  }
}
