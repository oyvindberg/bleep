package bleepscript;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

public record ScalaConfig(
    Optional<String> version,
    List<String> options,
    Set<Dep> compilerPlugins,
    Optional<CompileSetup> setup,
    Optional<Boolean> strict) {
  public ScalaConfig {
    Objects.requireNonNull(version, "version");
    Objects.requireNonNull(options, "options");
    Objects.requireNonNull(compilerPlugins, "compilerPlugins");
    Objects.requireNonNull(setup, "setup");
    Objects.requireNonNull(strict, "strict");
    options = List.copyOf(options);
    compilerPlugins = Set.copyOf(compilerPlugins);
  }
}
