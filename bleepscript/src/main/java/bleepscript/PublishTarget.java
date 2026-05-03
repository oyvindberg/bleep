package bleepscript;

import java.nio.file.Path;
import java.util.Objects;

public sealed interface PublishTarget permits PublishTarget.LocalIvy, PublishTarget.MavenFolder {
  final class LocalIvy implements PublishTarget {
    public static final LocalIvy INSTANCE = new LocalIvy();

    private LocalIvy() {}
  }

  record MavenFolder(Path path) implements PublishTarget {
    public MavenFolder {
      Objects.requireNonNull(path, "path");
    }
  }
}
