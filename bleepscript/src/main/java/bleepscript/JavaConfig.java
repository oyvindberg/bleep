package bleepscript;

import java.util.List;
import java.util.Objects;

public record JavaConfig(List<String> options) {
  public JavaConfig {
    Objects.requireNonNull(options, "options");
    options = List.copyOf(options);
  }
}
