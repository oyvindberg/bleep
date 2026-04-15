package bleepscript;

import java.nio.file.Path;
import java.util.List;

public interface Started {
  Logger logger();

  Build build();

  BuildPaths buildPaths();

  UserPaths userPaths();

  ProjectPaths projectPaths(CrossProjectName cross);

  Path jvmCommand();

  ResolvedJvm resolvedJvm();

  Path fetchNode(String nodeVersion);

  List<CrossProjectName> activeProjects();

  Project exploded(CrossProjectName cross);

  ResolvedProject resolved(CrossProjectName cross);

  Path bleepExecutable();
}
