package bleepscript;

import java.util.Objects;
import java.util.Set;

public record ScriptDef(CrossProjectName project, String main, Set<RelPath> sourceGlobs) {
  public ScriptDef {
    Objects.requireNonNull(project, "project");
    Objects.requireNonNull(main, "main");
    Objects.requireNonNull(sourceGlobs, "sourceGlobs");
    sourceGlobs = Set.copyOf(sourceGlobs);
  }
}
