package bleepscript;

import java.nio.file.Path;
import java.util.Objects;
import java.util.Optional;

public record ResolvedJvm(String name, Optional<String> index, Path javaBin) {
  public ResolvedJvm {
    Objects.requireNonNull(name, "name");
    Objects.requireNonNull(index, "index");
    Objects.requireNonNull(javaBin, "javaBin");
  }
}
