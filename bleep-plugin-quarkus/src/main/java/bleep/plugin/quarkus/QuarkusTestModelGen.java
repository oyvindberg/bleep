package bleep.plugin.quarkus;

import bleepscript.BleepCodegenScript;
import bleepscript.CodegenTarget;
import bleepscript.Commands;
import bleepscript.CrossProjectName;
import bleepscript.Started;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/**
 * Sourcegen script that makes {@code @QuarkusTest} work in a bleep build. Declare it on the Quarkus
 * test project (typically via a shared template):
 *
 * <pre>{@code
 * templates:
 *   quarkus-test:
 *     isTestProject: true
 *     sourcegen:
 *       main: bleep.plugin.quarkus.QuarkusTestModelGen
 *       project: scripts
 * projects:
 *   myapp-test:
 *     dependsOn: myapp
 *     dependencies: io.quarkus:quarkus-junit5:<version>
 *     extends: quarkus-test
 * }</pre>
 *
 * <p>That is the whole build-side surface — no {@code platform} block. Sourcegen runs before the
 * project compiles, which is early enough: the serialized application model records classpaths and
 * directories, not class file contents. The script writes {@code <target
 * dir>/quarkus/test-app-model.dat} (cached on a hash of everything that feeds it), then declares
 * the two JVM options the test fork needs by writing them to the project's {@code forkJvmOptions}
 * file, which bleep appends when it assembles the fork:
 *
 * <ul>
 *   <li>{@code quarkus-internal-test.serialized-app-model.path} — Quarkus's own escape hatch,
 *       checked before any Maven/Gradle build parsing (it is how the Gradle plugin feeds test JVMs)
 *   <li>the jboss LogManager, which must be installed before any JUL initialization
 * </ul>
 *
 * <p>Everything else — workspace projects indexed as application archives, BOM constraints,
 * extension flags — travels inside the serialized model itself. No directory-layout mapping is
 * needed either: bleep names a test project's output dir {@code test-classes}, which Quarkus's
 * {@code PathTestHelper} already recognizes via its built-in Maven fragment, and the application's
 * real paths come from the serialized model.
 */
public class QuarkusTestModelGen extends BleepCodegenScript {

  public QuarkusTestModelGen() {
    super("quarkus-test-model");
  }

  @Override
  public void run(
      Started started, Commands commands, List<CodegenTarget> targets, List<String> args) {
    // Each project's model is built by forking a Quarkus bootstrap JVM — tens of seconds of work,
    // and
    // on a cold compile the whole build's critical path. The projects are independent (their own
    // classpath, their own output file), so build them concurrently instead of one after another.
    // On
    // a two-project build (a typical Quarkus app pair) this roughly halves the sourcegen's wall
    // time.
    targets.parallelStream().forEach(target -> buildTestModel(started, target));
  }

  private static void buildTestModel(Started started, CodegenTarget target) {
    CrossProjectName testProject = target.project();
    if (!QuarkusAppModel.isQuarkusTestProject(started, testProject)) {
      String resolvedModules =
          started
              .resolved(testProject)
              .resolution()
              .map(
                  r ->
                      r.modules().stream()
                          .map(m -> m.organization() + ":" + m.name() + ":" + m.version())
                          .sorted()
                          .collect(java.util.stream.Collectors.joining(", ")))
              .orElse("<no resolution available>");
      throw new IllegalStateException(
          testProject.asString()
              + " declares the QuarkusTestModelGen sourcegen but does not resolve"
              + " io.quarkus:quarkus-junit5; either add the dependency or remove the sourcegen."
              + " Resolved modules: "
              + resolvedModules);
    }
    Path modelFile = QuarkusAppModel.ensureTestModel(started, testProject);
    writeForkOptions(started, testProject, modelFile);
  }

  /**
   * Declare the fork's JVM options for bleep to pick up. The serialized-model path is absolute
   * (bleep appends these verbatim, with no {@code ${...}} expansion), so it points straight at the
   * file this run produced. The jboss LogManager must be installed before any JUL initialization —
   * Quarkus requires the identical setting under surefire.
   */
  private static void writeForkOptions(
      Started started, CrossProjectName testProject, Path modelFile) {
    Path optionsFile = started.projectPaths(testProject).forkJvmOptions();
    String content =
        "-Djava.util.logging.manager=org.jboss.logmanager.LogManager\n"
            + "-Dquarkus-internal-test.serialized-app-model.path="
            + modelFile
            + "\n";
    try {
      Files.createDirectories(optionsFile.getParent());
      Files.writeString(optionsFile, content, StandardCharsets.UTF_8);
    } catch (IOException e) {
      throw new UncheckedIOException("writing " + optionsFile, e);
    }
  }
}
