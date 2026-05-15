package bleepscript;

import java.nio.file.Path;
import java.util.Objects;

/**
 * Filesystem paths bleep uses to organize state for one workspace + variant. Exposed to scripts so
 * they can locate inputs and outputs without hardcoding `.bleep/...` segments.
 *
 * <p>Two top-level categories:
 *
 * <ul>
 *   <li><b>Per cross-project state</b> lives under {@code projectsDir / <crossName>}. Use {@link
 *       #crossProjectDir} and {@link #variantBuildDir} to locate a specific project's directory.
 *   <li><b>Workspace-level state</b> spans projects: {@link #workspaceBuildsDir}, {@link
 *       #workspaceVariantDir}, {@link #logFile}.
 * </ul>
 */
public record BuildPaths(
    Path cwd,
    Path bleepYamlFile,
    Path buildDir,
    Path dotBleepDir,
    Path projectsDir,
    Path workspaceBuildsDir,
    Path workspaceVariantDir,
    Path logFile) {
  public BuildPaths {
    Objects.requireNonNull(cwd, "cwd");
    Objects.requireNonNull(bleepYamlFile, "bleepYamlFile");
    Objects.requireNonNull(buildDir, "buildDir");
    Objects.requireNonNull(dotBleepDir, "dotBleepDir");
    Objects.requireNonNull(projectsDir, "projectsDir");
    Objects.requireNonNull(workspaceBuildsDir, "workspaceBuildsDir");
    Objects.requireNonNull(workspaceVariantDir, "workspaceVariantDir");
    Objects.requireNonNull(logFile, "logFile");
  }

  /** Per cross-project root directory: {@code .bleep/projects/<crossName>}. */
  public Path crossProjectDir(CrossProjectName crossName) {
    return projectsDir.resolve(crossName.asString());
  }

  /**
   * Per-variant build state for one cross-project: {@code
   * .bleep/projects/<crossName>/builds/<variant>}. Contains compile output, Zinc analysis, KSP
   * per-variant state.
   */
  public Path variantBuildDir(CrossProjectName crossName) {
    return crossProjectDir(crossName)
        .resolve("builds")
        .resolve(workspaceVariantDir.getFileName().toString());
  }

  /**
   * Source-like generated outputs for one cross-project, shared across variants. Use this for
   * sourcegen output paths, annotation-processing output, KSP source emissions.
   */
  public Path generatedSourcesDir(CrossProjectName crossName, String folderName) {
    return crossProjectDir(crossName).resolve("generated-sources").resolve(folderName);
  }

  /** Resource-like generated outputs for one cross-project, shared across variants. */
  public Path generatedResourcesDir(CrossProjectName crossName, String folderName) {
    return crossProjectDir(crossName).resolve("generated-resources").resolve(folderName);
  }
}
