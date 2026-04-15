package bleepscript;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

public record KotlinConfig(Optional<String> version, List<String> options) {
  public KotlinConfig {
    Objects.requireNonNull(version, "version");
    Objects.requireNonNull(options, "options");
    options = List.copyOf(options);
  }
}
