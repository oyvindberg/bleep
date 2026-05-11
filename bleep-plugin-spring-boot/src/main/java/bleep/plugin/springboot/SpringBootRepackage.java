package bleep.plugin.springboot;

import bleepscript.*;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.time.Instant;
import java.util.*;
import java.util.jar.Attributes;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import org.springframework.boot.loader.tools.*;

/**
 * Produces a Spring Boot executable fat JAR for a project. Equivalent to {@code mvn
 * spring-boot:repackage} except it doesn't need a Maven {@code package} phase first — the source
 * JAR is built from the project's class output directly.
 *
 * <p>Output: {@code <workspace>/build/<project>/<project>-boot.jar} (configurable). The original
 * compiled classes directory is untouched; nothing in {@code target/} is mutated.
 *
 * <p>Two usage modes (same as {@link SpringBootRun}): reference directly as a script's {@code
 * main}, or wrap in a custom {@link BleepScript} that fluent-configures and calls {@link
 * #repackageOn}.
 */
public class SpringBootRepackage extends BleepScript {

  /** Spring Boot layout selector. Mirrors the Maven plugin's {@code <layout>} parameter. */
  public enum Layout {
    JAR(new Layouts.Jar()),
    WAR(new Layouts.War()),
    ZIP(new Layouts.Expanded()),
    NONE(new Layouts.None());

    private final org.springframework.boot.loader.tools.Layout impl;

    Layout(org.springframework.boot.loader.tools.Layout impl) {
      this.impl = impl;
    }
  }

  private Layout layout = Layout.JAR;
  private boolean layered = false;
  private boolean excludeDevtools = true;
  private boolean excludeDockerCompose = true;
  private final Set<String> requiresUnpackCoords = new LinkedHashSet<>();
  private Instant reproducibleTimestamp;
  private Path outputFile;
  private String mainClassOverride;
  private boolean includeTools = true;

  public SpringBootRepackage() {
    super("springboot-repackage");
  }

  public SpringBootRepackage withLayout(Layout layout) {
    this.layout = layout;
    return this;
  }

  /** Write {@code layers.idx} for layered Docker images. Default false. */
  public SpringBootRepackage withLayered(boolean layered) {
    this.layered = layered;
    return this;
  }

  /** Strip {@code spring-boot-devtools} from the repackaged jar. Default true. */
  public SpringBootRepackage withExcludeDevtools(boolean exclude) {
    this.excludeDevtools = exclude;
    return this;
  }

  /** Strip {@code spring-boot-docker-compose} from the repackaged jar. Default true. */
  public SpringBootRepackage withExcludeDockerCompose(boolean exclude) {
    this.excludeDockerCompose = exclude;
    return this;
  }

  /** Coordinate fragments matched against filenames; matching libs are unpacked at runtime. */
  public SpringBootRepackage withRequiresUnpack(String... coords) {
    this.requiresUnpackCoords.addAll(Arrays.asList(coords));
    return this;
  }

  /** Reproducible build timestamp. If unset, file timestamps are not normalized. */
  public SpringBootRepackage withReproducibleTimestamp(Instant timestamp) {
    this.reproducibleTimestamp = timestamp;
    return this;
  }

  /** Override the auto-derived output path. */
  public SpringBootRepackage withOutputFile(Path outputFile) {
    this.outputFile = outputFile;
    return this;
  }

  /** Override the project's declared main class. */
  public SpringBootRepackage withMainClass(String mainClass) {
    this.mainClassOverride = mainClass;
    return this;
  }

  /** Include Spring's loader tools jar. Default true (matches Maven plugin). */
  public SpringBootRepackage withIncludeTools(boolean include) {
    this.includeTools = include;
    return this;
  }

  @Override
  public void run(Started started, Commands commands, List<String> args) {
    if (args.isEmpty()) {
      throw new IllegalArgumentException(
          "springboot-repackage requires a project name as the first argument");
    }
    repackageOn(started, commands, args.get(0));
  }

  /**
   * Produce the executable fat JAR for the given project. Compiles first, builds a source JAR from
   * the project's class output and resources, and hands it to Spring Boot's {@link Repackager}.
   *
   * @return the path to the produced fat JAR
   */
  public Path repackageOn(Started started, Commands commands, String projectName) {
    CrossProjectName cross = CrossProjectName.of(projectName);
    commands.compile(List.of(cross));

    ResolvedProject resolved = started.resolved(cross);
    Project exploded = started.exploded(cross);
    String mainClass = resolveMainClass(exploded, projectName);

    Path target = resolveOutputFile(resolved, projectName);
    try {
      Files.createDirectories(target.getParent());
    } catch (IOException e) {
      throw new RuntimeException("Could not create output directory " + target.getParent(), e);
    }

    // Build a conventional source JAR from classes + resources. Repackager mutates this in place.
    Path sourceJar;
    try {
      sourceJar = Files.createTempFile("bleep-springboot-source-", ".jar");
      buildSourceJar(resolved, mainClass, sourceJar);
    } catch (IOException e) {
      throw new RuntimeException("Could not build source jar for " + projectName, e);
    }

    Libraries libraries = buildLibraries(resolved);

    Repackager repackager = new Repackager(sourceJar.toFile());
    repackager.setMainClass(mainClass);
    repackager.setLayout(layout.impl);
    repackager.setBackupSource(false);
    if (layered) {
      repackager.setLayers(Layers.IMPLICIT);
    }
    repackager.setIncludeRelevantJarModeJars(includeTools);

    try {
      FileTime timestamp =
          reproducibleTimestamp == null ? null : FileTime.from(reproducibleTimestamp);
      repackager.repackage(target.toFile(), libraries, null, timestamp);
    } catch (IOException e) {
      throw new RuntimeException("Repackage failed for " + projectName, e);
    } finally {
      try {
        Files.deleteIfExists(sourceJar);
      } catch (IOException ignored) {
      }
    }

    started.logger().info("Wrote " + target);
    return target;
  }

  private Path resolveOutputFile(ResolvedProject resolved, String projectName) {
    if (outputFile != null) {
      return outputFile;
    }
    return resolved
        .workspaceDir()
        .resolve("build")
        .resolve(projectName)
        .resolve(projectName + "-boot.jar");
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
                        + "set it, or call .withMainClass(...) on SpringBootRepackage."));
  }

  private void buildSourceJar(ResolvedProject resolved, String mainClass, Path target)
      throws IOException {
    Manifest manifest = new Manifest();
    Attributes attrs = manifest.getMainAttributes();
    attrs.put(Attributes.Name.MANIFEST_VERSION, "1.0");
    attrs.putValue("Created-By", "bleep-plugin-spring-boot");
    attrs.put(Attributes.Name.MAIN_CLASS, mainClass);

    Set<String> written = new LinkedHashSet<>();
    try (OutputStream out = Files.newOutputStream(target);
        BufferedOutputStream bos = new BufferedOutputStream(out);
        JarOutputStream jar = new JarOutputStream(bos, manifest)) {
      writeTree(jar, resolved.classesDir(), written);
      if (resolved.resources().isPresent()) {
        for (Path resourceDir : resolved.resources().get()) {
          writeTree(jar, resourceDir, written);
        }
      }
    }
  }

  private void writeTree(JarOutputStream jar, Path root, Set<String> written) throws IOException {
    if (!Files.isDirectory(root)) {
      return;
    }
    Files.walkFileTree(
        root,
        new SimpleFileVisitor<>() {
          @Override
          public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
              throws IOException {
            String entry = root.relativize(file).toString().replace('\\', '/');
            if (entry.isEmpty() || entry.equals("META-INF/MANIFEST.MF")) {
              return FileVisitResult.CONTINUE;
            }
            if (!written.add(entry)) {
              return FileVisitResult.CONTINUE;
            }
            JarEntry je = new JarEntry(entry);
            je.setTime(attrs.lastModifiedTime().toMillis());
            jar.putNextEntry(je);
            Files.copy(file, jar);
            jar.closeEntry();
            return FileVisitResult.CONTINUE;
          }
        });
  }

  private Libraries buildLibraries(ResolvedProject resolved) {
    List<Path> jars = new ArrayList<>(resolved.classpath().size());
    Path classesDir = resolved.classesDir();
    for (Path entry : resolved.classpath()) {
      if (entry.equals(classesDir)) {
        continue;
      }
      String name = entry.getFileName().toString();
      if (excludeDevtools && name.contains("spring-boot-devtools")) {
        continue;
      }
      if (excludeDockerCompose && name.contains("spring-boot-docker-compose")) {
        continue;
      }
      if (Files.isRegularFile(entry) && (name.endsWith(".jar") || name.endsWith(".zip"))) {
        jars.add(entry);
      }
    }

    final List<Path> finalJars = Collections.unmodifiableList(jars);
    final Set<String> unpackMatchers = Set.copyOf(requiresUnpackCoords);

    return callback -> {
      for (Path jar : finalJars) {
        boolean unpack = unpackMatchers.stream().anyMatch(m -> jar.toString().contains(m));
        callback.library(
            new Library(null, jar.toFile(), LibraryScope.COMPILE, null, unpack, false, true));
      }
    };
  }
}
