package bleep.quarkus;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * The spec file bleep hands to {@link QuarkusModelWriter}: everything bleep knows and Quarkus
 * needs, in a line-oriented, tab-separated format.
 *
 * <p>Why not JSON: this JVM's classpath already carries the app's own Quarkus jars, and any JSON
 * library we added could collide with something an extension drags in. Tab-separated lines need no
 * dependency and no escaping (a tab in a file path is rejected on the producing side, where the
 * error is actionable).
 *
 * <p>Line grammar, one record per line, first field is the tag:
 *
 * <pre>
 * mode                 test | dev | prod
 * app                  groupId  artifactId  version
 * module-dir           path
 * build-dir            path
 * build-file           path                                (the workspace's bleep.yaml)
 * main-classes         path
 * main-source-dir      path                                (0..n)
 * main-resource-dir    path                                (0..n; bleep resources are source dirs served as-is)
 * test-classes         path                                (0..1; required when mode=test)
 * test-source-dir      path                                (0..n)
 * test-resource-dir    path                                (0..n)
 * repo                 maven repo url                      (0..n; central is always added)
 * direct               groupId:artifactId                  (0..n; marks DIRECT deps)
 * dep                  groupId  artifactId  classifier|-  version  jarPath   (runtime classpath, in order)
 * project-dep          name  path                          (0..n; same name accumulates paths, first is classes)
 * output               path
 * </pre>
 */
public final class ModelSpec {
  public enum Mode {
    TEST,
    DEV,
    PROD
  }

  public record Gav(String groupId, String artifactId, String classifier, String version) {
    @Override
    public String toString() {
      return groupId
          + ":"
          + artifactId
          + (classifier.isEmpty() ? "" : ":" + classifier)
          + ":"
          + version;
    }
  }

  public record Dep(Gav gav, Path path) {}

  public record ProjectDep(String name, List<Path> paths) {}

  public final Mode mode;
  public final String appGroupId;
  public final String appArtifactId;
  public final String appVersion;
  public final Path moduleDir;
  public final Path buildDir;
  public final Path buildFile;
  public final Path mainClasses;
  public final List<Path> mainSourceDirs;
  public final List<Path> mainResourceDirs;
  public final Path testClasses; // null unless mode == TEST
  public final List<Path> testSourceDirs;
  public final List<Path> testResourceDirs;
  public final List<String> repos;
  public final List<String> directDeps; // "groupId:artifactId"
  public final List<Dep> deps; // runtime classpath order
  public final List<ProjectDep> projectDeps;
  public final Path output;

  private ModelSpec(
      Mode mode,
      String appGroupId,
      String appArtifactId,
      String appVersion,
      Path moduleDir,
      Path buildDir,
      Path buildFile,
      Path mainClasses,
      List<Path> mainSourceDirs,
      List<Path> mainResourceDirs,
      Path testClasses,
      List<Path> testSourceDirs,
      List<Path> testResourceDirs,
      List<String> repos,
      List<String> directDeps,
      List<Dep> deps,
      List<ProjectDep> projectDeps,
      Path output) {
    this.mode = mode;
    this.appGroupId = appGroupId;
    this.appArtifactId = appArtifactId;
    this.appVersion = appVersion;
    this.moduleDir = moduleDir;
    this.buildDir = buildDir;
    this.buildFile = buildFile;
    this.mainClasses = mainClasses;
    this.mainSourceDirs = mainSourceDirs;
    this.mainResourceDirs = mainResourceDirs;
    this.testClasses = testClasses;
    this.testSourceDirs = testSourceDirs;
    this.testResourceDirs = testResourceDirs;
    this.repos = repos;
    this.directDeps = directDeps;
    this.deps = deps;
    this.projectDeps = projectDeps;
    this.output = output;
  }

  public static ModelSpec parse(Path file) throws IOException {
    Mode mode = null;
    String appGroupId = null;
    String appArtifactId = null;
    String appVersion = null;
    Path moduleDir = null;
    Path buildDir = null;
    Path buildFile = null;
    Path mainClasses = null;
    List<Path> mainSourceDirs = new ArrayList<>();
    List<Path> mainResourceDirs = new ArrayList<>();
    Path testClasses = null;
    List<Path> testSourceDirs = new ArrayList<>();
    List<Path> testResourceDirs = new ArrayList<>();
    List<String> repos = new ArrayList<>();
    List<String> directDeps = new ArrayList<>();
    List<Dep> deps = new ArrayList<>();
    Map<String, List<Path>> projectDeps = new LinkedHashMap<>();
    Path output = null;

    int lineNo = 0;
    for (String line : Files.readAllLines(file, StandardCharsets.UTF_8)) {
      lineNo++;
      if (line.isEmpty()) continue;
      String[] f = line.split("\t", -1);
      switch (f[0]) {
        case "mode" -> mode = Mode.valueOf(req(f, 1, lineNo).toUpperCase(java.util.Locale.ROOT));
        case "app" -> {
          appGroupId = req(f, 1, lineNo);
          appArtifactId = req(f, 2, lineNo);
          appVersion = req(f, 3, lineNo);
        }
        case "module-dir" -> moduleDir = Path.of(req(f, 1, lineNo));
        case "build-dir" -> buildDir = Path.of(req(f, 1, lineNo));
        case "build-file" -> buildFile = Path.of(req(f, 1, lineNo));
        case "main-classes" -> mainClasses = Path.of(req(f, 1, lineNo));
        case "main-source-dir" -> mainSourceDirs.add(Path.of(req(f, 1, lineNo)));
        case "main-resource-dir" -> mainResourceDirs.add(Path.of(req(f, 1, lineNo)));
        case "test-classes" -> testClasses = Path.of(req(f, 1, lineNo));
        case "test-source-dir" -> testSourceDirs.add(Path.of(req(f, 1, lineNo)));
        case "test-resource-dir" -> testResourceDirs.add(Path.of(req(f, 1, lineNo)));
        case "repo" -> repos.add(req(f, 1, lineNo));
        case "direct" -> directDeps.add(req(f, 1, lineNo));
        case "dep" -> {
          String classifier = req(f, 3, lineNo);
          deps.add(
              new Dep(
                  new Gav(
                      req(f, 1, lineNo),
                      req(f, 2, lineNo),
                      "-".equals(classifier) ? "" : classifier,
                      req(f, 4, lineNo)),
                  Path.of(req(f, 5, lineNo))));
        }
        case "project-dep" ->
            projectDeps
                .computeIfAbsent(req(f, 1, lineNo), k -> new ArrayList<>())
                .add(Path.of(req(f, 2, lineNo)));
        case "output" -> output = Path.of(req(f, 1, lineNo));
        default ->
            throw new IllegalArgumentException(
                file + ":" + lineNo + ": unknown record tag '" + f[0] + "'");
      }
    }

    if (mode == null) throw new IllegalArgumentException(file + ": missing 'mode'");
    if (appGroupId == null) throw new IllegalArgumentException(file + ": missing 'app'");
    if (moduleDir == null) throw new IllegalArgumentException(file + ": missing 'module-dir'");
    if (buildDir == null) throw new IllegalArgumentException(file + ": missing 'build-dir'");
    if (buildFile == null) throw new IllegalArgumentException(file + ": missing 'build-file'");
    if (mainClasses == null) throw new IllegalArgumentException(file + ": missing 'main-classes'");
    if (output == null) throw new IllegalArgumentException(file + ": missing 'output'");
    if (mode == Mode.TEST && testClasses == null)
      throw new IllegalArgumentException(file + ": mode=test requires 'test-classes'");

    List<ProjectDep> pdeps = new ArrayList<>(projectDeps.size());
    projectDeps.forEach((name, paths) -> pdeps.add(new ProjectDep(name, List.copyOf(paths))));

    return new ModelSpec(
        mode,
        appGroupId,
        appArtifactId,
        appVersion,
        moduleDir,
        buildDir,
        buildFile,
        mainClasses,
        List.copyOf(mainSourceDirs),
        List.copyOf(mainResourceDirs),
        testClasses,
        List.copyOf(testSourceDirs),
        List.copyOf(testResourceDirs),
        List.copyOf(repos),
        List.copyOf(directDeps),
        List.copyOf(deps),
        List.copyOf(pdeps),
        output);
  }

  private static String req(String[] fields, int idx, int lineNo) {
    if (idx >= fields.length)
      throw new IllegalArgumentException(
          "line " + lineNo + ": expected at least " + (idx + 1) + " fields, got " + fields.length);
    String v = fields[idx];
    if (v.isEmpty())
      throw new IllegalArgumentException("line " + lineNo + ": field " + idx + " is empty");
    return v;
  }
}
