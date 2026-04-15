package bleepscript;

import java.nio.file.Path;
import java.util.Objects;

public record CodegenTarget(CrossProjectName project, Path sources, Path resources) {
  public CodegenTarget {
    Objects.requireNonNull(project, "project");
    Objects.requireNonNull(sources, "sources");
    Objects.requireNonNull(resources, "resources");
  }
}
