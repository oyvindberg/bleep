package bleepscript;

import java.nio.file.Path;
import java.util.Objects;

public record BuildPaths(
    Path cwd,
    Path bleepYamlFile,
    Path buildDir,
    Path dotBleepDir,
    Path buildsDir,
    Path buildVariantDir,
    Path bleepBloopDir,
    Path logFile) {
  public BuildPaths {
    Objects.requireNonNull(cwd, "cwd");
    Objects.requireNonNull(bleepYamlFile, "bleepYamlFile");
    Objects.requireNonNull(buildDir, "buildDir");
    Objects.requireNonNull(dotBleepDir, "dotBleepDir");
    Objects.requireNonNull(buildsDir, "buildsDir");
    Objects.requireNonNull(buildVariantDir, "buildVariantDir");
    Objects.requireNonNull(bleepBloopDir, "bleepBloopDir");
    Objects.requireNonNull(logFile, "logFile");
  }

  public Path generatedSourcesDir(CrossProjectName crossName, String folderName) {
    return dotBleepDir
        .resolve("generated-sources")
        .resolve(crossName.asString())
        .resolve(folderName);
  }

  public Path generatedResourcesDir(CrossProjectName crossName, String folderName) {
    return dotBleepDir
        .resolve("generated-resources")
        .resolve(crossName.asString())
        .resolve(folderName);
  }
}
