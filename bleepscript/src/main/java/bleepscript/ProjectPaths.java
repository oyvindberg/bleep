package bleepscript;

import java.nio.file.Path;
import java.util.List;
import java.util.Objects;

public record ProjectPaths(
    Path dir,
    Path targetDir,
    Path classes,
    Path incrementalAnalysis,
    List<Path> sourceDirs,
    List<Path> resourceDirs,
    boolean isTestProject) {
  public ProjectPaths {
    Objects.requireNonNull(dir, "dir");
    Objects.requireNonNull(targetDir, "targetDir");
    Objects.requireNonNull(classes, "classes");
    Objects.requireNonNull(incrementalAnalysis, "incrementalAnalysis");
    Objects.requireNonNull(sourceDirs, "sourceDirs");
    Objects.requireNonNull(resourceDirs, "resourceDirs");
    sourceDirs = List.copyOf(sourceDirs);
    resourceDirs = List.copyOf(resourceDirs);
  }
}
