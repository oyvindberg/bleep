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

  /**
   * The JDK/JRE home directory, i.e. the parent of {@code bin/}. Suitable for {@code JAVA_HOME} env
   * vars and for tool config that wants paths like {@code <home>/jmods/java.base.jmod}.
   */
  public Path javaHome() {
    return javaBin.getParent().getParent();
  }
}
