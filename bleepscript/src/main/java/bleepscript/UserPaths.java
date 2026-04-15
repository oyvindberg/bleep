package bleepscript;

import java.nio.file.Path;
import java.util.Objects;

public record UserPaths(
    Path cacheDir,
    Path configDir,
    Path bspSocketDir,
    Path resolveCacheDir,
    Path resolveJvmCacheDir,
    Path configYaml) {
  public UserPaths {
    Objects.requireNonNull(cacheDir, "cacheDir");
    Objects.requireNonNull(configDir, "configDir");
    Objects.requireNonNull(bspSocketDir, "bspSocketDir");
    Objects.requireNonNull(resolveCacheDir, "resolveCacheDir");
    Objects.requireNonNull(resolveJvmCacheDir, "resolveJvmCacheDir");
    Objects.requireNonNull(configYaml, "configYaml");
  }
}
