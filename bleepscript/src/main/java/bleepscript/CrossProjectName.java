package bleepscript;

import java.util.Objects;
import java.util.Optional;

public record CrossProjectName(String name, Optional<String> crossId) {
  public CrossProjectName {
    Objects.requireNonNull(name, "name");
    Objects.requireNonNull(crossId, "crossId");
  }

  public static CrossProjectName of(String name) {
    return new CrossProjectName(name, Optional.empty());
  }

  public static CrossProjectName of(String name, String crossId) {
    return new CrossProjectName(name, Optional.of(crossId));
  }

  public String asString() {
    return crossId.map(cid -> name + "@" + cid).orElse(name);
  }

  @Override
  public String toString() {
    return asString();
  }
}
