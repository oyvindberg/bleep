package bleepscript;

import java.net.URI;
import java.nio.file.Path;
import java.util.Objects;
import java.util.Optional;

public sealed interface Repository
    permits Repository.Maven, Repository.MavenFolder, Repository.Ivy {
  Optional<String> name();

  record Maven(Optional<String> name, URI uri) implements Repository {
    public Maven {
      Objects.requireNonNull(name, "name");
      Objects.requireNonNull(uri, "uri");
    }
  }

  record MavenFolder(Optional<String> name, Path path) implements Repository {
    public MavenFolder {
      Objects.requireNonNull(name, "name");
      Objects.requireNonNull(path, "path");
    }
  }

  record Ivy(Optional<String> name, URI uri) implements Repository {
    public Ivy {
      Objects.requireNonNull(name, "name");
      Objects.requireNonNull(uri, "uri");
    }
  }
}
