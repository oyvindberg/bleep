package bleepscript;

import java.util.List;
import java.util.Map;
import java.util.Objects;

public record Build(
    BleepVersion version,
    Map<CrossProjectName, Project> explodedProjects,
    List<Repository> resolvers,
    Map<String, List<ScriptDef>> scripts) {

  public Build {
    Objects.requireNonNull(version, "version");
    Objects.requireNonNull(explodedProjects, "explodedProjects");
    Objects.requireNonNull(resolvers, "resolvers");
    Objects.requireNonNull(scripts, "scripts");
    explodedProjects = Map.copyOf(explodedProjects);
    resolvers = List.copyOf(resolvers);
    scripts = Map.copyOf(scripts);
  }
}
