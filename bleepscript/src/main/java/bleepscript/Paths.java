package bleepscript;

import java.nio.file.Path;
import java.util.Objects;

public final class Paths {
  private Paths() {}

  public static Path resolve(Path base, String rel) {
    Objects.requireNonNull(base, "base");
    Objects.requireNonNull(rel, "rel");
    return base.resolve(rel);
  }

  public static Path resolve(Path base, RelPath rel) {
    Objects.requireNonNull(base, "base");
    Objects.requireNonNull(rel, "rel");
    Path result = base;
    for (String segment : rel.segments()) {
      result = result.resolve(segment);
    }
    return result;
  }
}
