package bleepscript;

import java.nio.file.Path;

/**
 * Filesystem paths bleep uses to organize state for one workspace + variant. Exposed to scripts so
 * they can locate inputs and outputs without hardcoding {@code .bleep/...} segments.
 *
 * <p>Two top-level categories:
 *
 * <ul>
 *   <li><b>Per cross-project state</b> lives under {@link #projectsDir()} {@code / <crossName>}.
 *       Use {@link #crossProjectDir} and {@link #variantBuildDir} to locate a specific project's
 *       directory.
 *   <li><b>Workspace-level state</b> spans projects: {@link #workspaceBuildsDir()}, {@link
 *       #workspaceVariantDir()}, {@link #logFile()}.
 * </ul>
 *
 * <p>Implementations come from {@code bleep-core}'s {@code JModel.buildPaths(...)}, which delegates
 * each method to the underlying Scala {@code bleep.BuildPaths}. The Scala side is the single source
 * of truth for the on-disk layout — this Java interface is just the script-facing surface.
 */
public interface BuildPaths {
  Path cwd();

  Path bleepYamlFile();

  Path buildDir();

  Path dotBleepDir();

  /** Holds one subdirectory per cross-built project: {@code .bleep/projects/}. */
  Path projectsDir();

  /** Per-variant workspace-level state (build logs, etc.): {@code .bleep/builds/}. */
  Path workspaceBuildsDir();

  /** Per-variant scratch for this workspace: {@code .bleep/builds/<variantName>/}. */
  Path workspaceVariantDir();

  /**
   * Build log from the most recent compile in this variant: {@code workspaceVariantDir/last.log}.
   */
  Path logFile();

  /** Per cross-project root directory: {@code .bleep/projects/<crossName>}. */
  Path crossProjectDir(CrossProjectName crossName);

  /**
   * Per-variant build state for one cross-project: {@code
   * .bleep/projects/<crossName>/builds/<variantName>}. Contains compile output, Zinc analysis, KSP
   * per-variant state.
   */
  Path variantBuildDir(CrossProjectName crossName);

  /**
   * Source-like generated outputs for one cross-project, shared across variants. Use this for
   * sourcegen output paths, annotation-processing output, KSP source emissions.
   */
  Path generatedSourcesDir(CrossProjectName crossName, String folderName);

  /** Resource-like generated outputs for one cross-project, shared across variants. */
  Path generatedResourcesDir(CrossProjectName crossName, String folderName);
}
