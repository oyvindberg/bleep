package bleep.plugin.quarkus;

import bleepscript.Coursier;
import bleepscript.CrossProjectName;
import bleepscript.ProjectPaths;
import bleepscript.Repository;
import bleepscript.ResolvedProject;
import bleepscript.Started;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

/**
 * Produces the serialized Quarkus application model for a bleep project.
 *
 * <p>Quarkus bootstraps — for {@code @QuarkusTest}, dev mode and production packaging alike — by
 * building an "application model" of the app, and its stock resolvers parse {@code pom.xml} or
 * drive the Gradle tooling API, neither of which exists in a bleep workspace. Quarkus's own escape
 * hatch (the one its Gradle plugin uses) is a pre-serialized model handed over via system property,
 * checked before any build-tool detection. This class produces that file:
 *
 * <ul>
 *   <li>a spec file describing what bleep knows (classpath with coordinates, workspace layout,
 *       repositories), built entirely from the {@code bleepscript} API
 *   <li>a forked {@code bleep.quarkus.QuarkusModelWriter} JVM whose classpath carries the app's own
 *       Quarkus version, so the serialization format always matches what the consuming JVM reads
 *       back
 * </ul>
 *
 * <p>The model lands at the stable path {@code <target dir>/quarkus/<mode>-app-model.dat} — stable
 * because the project's {@code platform.jvmOptions} references it via {@code ${TARGET_DIR}} — with
 * a sibling {@code .inputs} hash guarding it, so the writer forks once per classpath or layout
 * change, not once per run.
 */
public final class QuarkusAppModel {
  private QuarkusAppModel() {}

  /** Keep in sync with the {@code io.get-coursier:interface} version in bleep's own bleep.yaml. */
  static final String COURSIER_INTERFACE_VERSION = "1.0.28";

  /** Build (or find cached) the test-mode model for a Quarkus test project. */
  public static Path ensureTestModel(Started started, CrossProjectName testProject) {
    CrossProjectName appProject = findAppProject(started, testProject);
    String spec = buildSpec(started, testProject, appProject, "test", true);
    return writeModel(started, testProject, "test", spec);
  }

  /** Build (or find cached) the dev- or prod-mode model for a Quarkus application project. */
  public static Path ensureAppModel(Started started, CrossProjectName appProject, String mode) {
    if (!mode.equals("dev") && !mode.equals("prod")) {
      throw new IllegalArgumentException("mode must be 'dev' or 'prod', got '" + mode + "'");
    }
    quarkusCoreVersion(
        started, appProject); // fail early, with the good message, on a non-Quarkus project
    String spec = buildSpec(started, appProject, appProject, mode, false);
    return writeModel(started, appProject, mode, spec);
  }

  /**
   * Classpath for forking the {@code bleep-quarkus} mains ({@code QuarkusModelWriter}, {@code
   * QuarkusDevRun}, {@code QuarkusPackager}): the bleep-quarkus artifact at this bleep's version
   * (via the {@code $&#123;BLEEP_VERSION&#125;} template, which a dev build short-circuits to class
   * dirs), coursier-interface pinned explicitly (the dev path carries no transitive jars), and
   * {@code quarkus-bootstrap-core} at the version the project resolved.
   */
  public static List<Path> runnerClasspath(Started started, CrossProjectName project) {
    String quarkusVersion = quarkusCoreVersion(started, project);
    LinkedHashSet<Path> cp = new LinkedHashSet<>();
    cp.addAll(Coursier.fetchClasspath(started, "build.bleep:bleep-quarkus:${BLEEP_VERSION}"));
    cp.addAll(
        Coursier.fetchClasspath(
            started, "io.get-coursier:interface:" + COURSIER_INTERFACE_VERSION));
    cp.addAll(
        Coursier.fetchClasspath(started, "io.quarkus:quarkus-bootstrap-core:" + quarkusVersion));
    return List.copyOf(cp);
  }

  static String quarkusCoreVersion(Started started, CrossProjectName project) {
    return resolutionOf(started, project).modules().stream()
        .filter(m -> m.organization().equals("io.quarkus") && m.name().equals("quarkus-core"))
        .map(ResolvedProject.ResolvedModule::version)
        .findFirst()
        .orElseThrow(
            () ->
                new IllegalStateException(
                    project.asString()
                        + " does not resolve io.quarkus:quarkus-core, so it is not a Quarkus"
                        + " application project"));
  }

  static boolean isQuarkusTestProject(Started started, CrossProjectName project) {
    // The test framework artifact is quarkus-junit5 up to Quarkus 3.3x and quarkus-junit after the
    // rename; a quarkus-junit5 dependency on a new version resolves to quarkus-junit via
    // relocation.
    return resolutionOf(started, project).modules().stream()
        .anyMatch(
            m ->
                m.organization().equals("io.quarkus")
                    && (m.name().equals("quarkus-junit5") || m.name().equals("quarkus-junit")));
  }

  private static ResolvedProject.Resolution resolutionOf(
      Started started, CrossProjectName project) {
    return started
        .resolved(project)
        .resolution()
        .orElseThrow(
            () ->
                new IllegalStateException(
                    project.asString() + ": no dependency resolution available"));
  }

  /**
   * The application project of a Quarkus test project: the direct {@code dependsOn} whose
   * resolution contains {@code io.quarkus:quarkus-core}. Anything else — zero candidates, several —
   * is a build shape this integration does not understand, and guessing would bootstrap the wrong
   * application.
   */
  static CrossProjectName findAppProject(Started started, CrossProjectName testProject) {
    Set<CrossProjectName> allProjects = started.build().explodedProjects().keySet();
    List<CrossProjectName> directDependsOn =
        started.exploded(testProject).dependsOn().stream()
            .map(
                pn ->
                    allProjects.stream()
                        .filter(
                            k -> k.name().equals(pn) && k.crossId().equals(testProject.crossId()))
                        .findFirst()
                        .or(() -> allProjects.stream().filter(k -> k.name().equals(pn)).findFirst())
                        .orElseThrow(
                            () ->
                                new IllegalStateException(
                                    testProject.asString()
                                        + ": dependsOn "
                                        + pn
                                        + " which does not resolve to a project")))
            .sorted(java.util.Comparator.comparing(CrossProjectName::asString))
            .collect(Collectors.toList());

    List<CrossProjectName> candidates =
        directDependsOn.stream()
            .filter(
                p ->
                    started.resolved(p).resolution().stream()
                        .anyMatch(
                            r ->
                                r.modules().stream()
                                    .anyMatch(
                                        m ->
                                            m.organization().equals("io.quarkus")
                                                && m.name().equals("quarkus-core"))))
            .collect(Collectors.toList());

    if (candidates.size() == 1) return candidates.get(0);
    if (candidates.size() > 1) {
      // Convention: `<app>-test dependsOn <app>`. When several direct deps resolve quarkus-core —
      // e.g.
      // the application plus a test-support project (an openapi client) that also pulls it in —
      // prefer
      // the candidate whose name, with `-test` appended, is exactly the test project's name.
      String testName = testProject.asString();
      List<CrossProjectName> byConvention =
          candidates.stream()
              .filter(c -> testName.equals(c.asString() + "-test"))
              .collect(Collectors.toList());
      if (byConvention.size() == 1) return byConvention.get(0);
    }
    String names =
        directDependsOn.stream().map(CrossProjectName::asString).collect(Collectors.joining(", "));
    if (candidates.isEmpty()) {
      throw new IllegalStateException(
          testProject.asString()
              + " resolves io.quarkus:quarkus-junit5 but none of its direct dependsOn projects ("
              + names
              + ") resolve io.quarkus:quarkus-core, so bleep cannot tell which project is the"
              + " Quarkus application");
    }
    throw new IllegalStateException(
        testProject.asString()
            + ": several direct dependsOn projects resolve io.quarkus:quarkus-core ("
            + candidates.stream().map(CrossProjectName::asString).collect(Collectors.joining(", "))
            + "); the Quarkus integration needs exactly one Quarkus application project per test"
            + " project");
  }

  /**
   * The spec is everything {@code bleep.quarkus.QuarkusModelWriter} needs, in its tab-separated
   * line format. {@code specOwner} is the project whose classpath becomes the model's runtime
   * classpath — the test project for test models, the app project itself otherwise. Deterministic
   * ordering throughout: the spec's hash is the model cache key.
   */
  private static String buildSpec(
      Started started,
      CrossProjectName specOwner,
      CrossProjectName appProject,
      String mode,
      boolean includeTest) {
    StringBuilder sb = new StringBuilder();

    ProjectPaths appPaths = started.projectPaths(appProject);
    ProjectPaths ownPaths = started.projectPaths(specOwner);

    line(sb, specOwner, "mode", mode);
    line(sb, specOwner, "app", "bleep.workspace", appProject.asString(), "0.0.0-bleep");
    line(sb, specOwner, "module-dir", appPaths.dir().toString());
    line(sb, specOwner, "build-dir", appPaths.targetDir().toString());
    line(
        sb,
        specOwner,
        "build-file",
        started.buildPaths().buildDir().resolve("bleep.yaml").toString());
    line(sb, specOwner, "main-classes", appPaths.classes().toString());
    for (Path p : appPaths.sourceDirs()) line(sb, specOwner, "main-source-dir", p.toString());
    for (Path p : appPaths.resourceDirs())
      if (existsOrIsWorkspaceOutput(started, p))
        line(sb, specOwner, "main-resource-dir", p.toString());
    if (includeTest) {
      line(sb, specOwner, "test-classes", ownPaths.classes().toString());
      for (Path p : ownPaths.sourceDirs()) line(sb, specOwner, "test-source-dir", p.toString());
      for (Path p : ownPaths.resourceDirs())
        if (existsOrIsWorkspaceOutput(started, p))
          line(sb, specOwner, "test-resource-dir", p.toString());
    }

    for (Repository r : started.build().resolvers()) {
      if (r instanceof Repository.Maven maven) line(sb, specOwner, "repo", maven.uri().toString());
      else if (r instanceof Repository.MavenFolder folder)
        line(sb, specOwner, "repo", folder.path().toUri().toString());
      else
        throw new IllegalStateException(
            specOwner.asString()
                + ": resolver "
                + r
                + " is not supported for Quarkus deployment classpath resolution");
    }

    // Direct dependencies of the involved projects mark the DIRECT flag in the model.
    Set<String> directDeps = new TreeSet<>();
    for (CrossProjectName p : includeTest ? List.of(specOwner, appProject) : List.of(specOwner)) {
      for (bleepscript.Dep dep : started.exploded(p).dependencies()) {
        directDeps.add(dep.organization() + ":" + dep.moduleName());
      }
    }
    for (String d : directDeps) line(sb, specOwner, "direct", d);

    // Walk the dependency classpath in order, classifying every entry: maven artifact, the
    // app/test projects themselves (skipped — they ARE the model's application artifact), another
    // bleep project, or unknown (an error: an unclassified entry would silently vanish from the
    // model).
    record Coords(String org, String name, Optional<String> classifier, String version) {}
    Map<Path, Coords> coordsByPath = new HashMap<>();
    for (ResolvedProject.ResolvedModule m : resolutionOf(started, specOwner).modules()) {
      for (ResolvedProject.ResolvedArtifact a : m.artifacts()) {
        coordsByPath.put(
            a.path(), new Coords(m.organization(), m.name(), a.classifier(), m.version()));
      }
    }

    Map<Path, CrossProjectName> projectByPath = new HashMap<>();
    for (CrossProjectName p : started.build().explodedProjects().keySet()) {
      ProjectPaths paths = started.projectPaths(p);
      projectByPath.put(paths.classes(), p);
      for (Path r : paths.resourceDirs()) projectByPath.put(r, p);
    }

    Set<Path> selfPaths = new HashSet<>();
    selfPaths.add(appPaths.classes());
    selfPaths.addAll(appPaths.resourceDirs());
    selfPaths.add(ownPaths.classes());
    selfPaths.addAll(ownPaths.resourceDirs());

    Set<String> seenProjectPaths = new HashSet<>();
    for (Path path : started.resolved(specOwner).classpath()) {
      Coords c = coordsByPath.get(path);
      if (c != null) {
        // The model puts bleep's own projects under the synthesized group `bleep.workspace`, and a
        // model-carried config default indexes that whole group as application archives. A real
        // maven artifact under the same group would collide with a project's coordinates in the
        // model and be silently indexed as application code — refuse instead of guessing.
        if (c.org().equals("bleep.workspace")) {
          throw new IllegalStateException(
              specOwner.asString()
                  + ": dependency "
                  + c.org()
                  + ":"
                  + c.name()
                  + " uses the group id bleep synthesizes for workspace projects in the Quarkus"
                  + " application model; this build cannot be modeled");
        }
        // resolved maven artifacts exist on disk by construction
        line(
            sb,
            specOwner,
            "dep",
            c.org(),
            c.name(),
            c.classifier().orElse("-"),
            c.version(),
            path.toString());
      } else if (selfPaths.contains(path)) {
        // the application artifact's own output; carried by the model's app coordinates instead
      } else {
        CrossProjectName p = projectByPath.get(path);
        if (p == null) {
          // bleep classpaths list configured directories whether or not they exist (an empty
          // resources dir, an as-yet-ungenerated output); an unclassifiable entry that isn't on
          // disk has nothing to contribute to the model.
          if (!Files.exists(path)) continue;
          throw new IllegalStateException(
              specOwner.asString()
                  + ": classpath entry "
                  + path
                  + " is neither a resolved artifact nor a bleep project output; refusing to build"
                  + " a Quarkus model that silently drops it");
        }
        // A project dependency's output dirs go into the model even when they do not exist YET
        // (see existsOrIsWorkspaceOutput). Dropping such an entry is how classes leak to the
        // parent classloader at runtime — LinkageError loader constraints on kotlin-stdlib
        // interfaces, invisible beans — but only on clean builds.
        if (!existsOrIsWorkspaceOutput(started, path)) continue;
        if (seenProjectPaths.add(p.asString() + " " + path)) {
          line(sb, specOwner, "project-dep", p.asString(), path.toString());
        }
      }
    }

    return sb.toString();
  }

  /**
   * Whether a configured directory belongs in the model. Source-tree dirs (a project's resources)
   * exist statically or not at all, so a missing one really is absent. Workspace OUTPUT dirs — a
   * dependency's classes, a sourcegen's generated-resources — may simply not have been produced
   * yet: this sourcegen runs before the dependency graph compiles on a clean build, while the model
   * is only read at test-JVM startup, after everything ran. Those stay in — and get created on the
   * spot, because Quarkus's PathTree refuses paths that are not on disk and nothing guarantees an
   * output dir nobody wrote to (a sourcegen that produced no resources) ever appears.
   */
  private static boolean existsOrIsWorkspaceOutput(Started started, Path path) {
    if (Files.exists(path)) return true;
    if (!path.startsWith(started.buildPaths().buildDir().resolve(".bleep"))) return false;
    try {
      Files.createDirectories(path);
    } catch (IOException e) {
      throw new java.io.UncheckedIOException("creating workspace output dir " + path, e);
    }
    return true;
  }

  private static void line(StringBuilder sb, CrossProjectName project, String... fields) {
    for (String f : fields) {
      if (f.contains("\t")) {
        throw new IllegalStateException(
            project.asString()
                + ": cannot express value containing a tab in the Quarkus model spec: "
                + f);
      }
    }
    sb.append(String.join("\t", fields)).append('\n');
  }

  /**
   * Serialize the model for {@code specContent} unless the {@code .inputs} guard says the existing
   * file was built from exactly this spec.
   */
  private static Path writeModel(
      Started started, CrossProjectName cacheOwner, String mode, String specContent) {
    Path quarkusDir = started.projectPaths(cacheOwner).targetDir().resolve("quarkus");
    Path datFile = quarkusDir.resolve(mode + "-app-model.dat");
    Path inputsFile = quarkusDir.resolve(mode + "-app-model.inputs");
    String hash = sha256(specContent);

    try {
      if (Files.exists(datFile)
          && Files.exists(inputsFile)
          && Files.readString(inputsFile).equals(hash)) {
        return datFile;
      }
      Files.createDirectories(quarkusDir);
      Files.deleteIfExists(inputsFile);
      Path specFile = quarkusDir.resolve(mode + "-model-spec.txt");
      Files.writeString(
          specFile, specContent + "output\t" + datFile + "\n", StandardCharsets.UTF_8);
      runModelWriter(started, cacheOwner, specFile);
      if (!Files.exists(datFile)) {
        throw new IllegalStateException(
            cacheOwner.asString()
                + ": QuarkusModelWriter exited successfully but did not produce "
                + datFile);
      }
      Files.writeString(inputsFile, hash, StandardCharsets.UTF_8);
      return datFile;
    } catch (IOException e) {
      throw new IllegalStateException(
          cacheOwner.asString() + ": failed to write Quarkus application model", e);
    }
  }

  private static void runModelWriter(Started started, CrossProjectName project, Path specFile) {
    List<Path> cp = runnerClasspath(started, project);
    List<String> cmd =
        List.of(
            started.jvmCommand().toString(),
            "-cp",
            cp.stream().map(Path::toString).collect(Collectors.joining(File.pathSeparator)),
            "bleep.quarkus.QuarkusModelWriter",
            specFile.toString());

    started.logger().info(project.asString() + ": building Quarkus application model");
    try {
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.redirectErrorStream(true);
      Process process = pb.start();
      String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
      int exit = process.waitFor();
      if (exit != 0) {
        throw new IllegalStateException(
            project.asString()
                + ": QuarkusModelWriter failed with exit code "
                + exit
                + ":\n"
                + output);
      }
      if (!output.isEmpty()) started.logger().debug("QuarkusModelWriter: " + output);
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      throw new IllegalStateException(
          project.asString() + ": interrupted while building Quarkus application model", e);
    } catch (IOException e) {
      throw new IllegalStateException(
          project.asString() + ": failed to fork QuarkusModelWriter", e);
    }
  }

  private static String sha256(String s) {
    try {
      byte[] digest =
          MessageDigest.getInstance("SHA-256").digest(s.getBytes(StandardCharsets.UTF_8));
      StringBuilder sb = new StringBuilder(digest.length * 2);
      for (byte b : digest) sb.append(String.format("%02x", b));
      return sb.toString();
    } catch (java.security.NoSuchAlgorithmException e) {
      throw new IllegalStateException("SHA-256 unavailable", e);
    }
  }
}
