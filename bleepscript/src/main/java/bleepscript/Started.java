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

  /**
   * The whole command that re-invokes this bleep, ready to hand to {@link Cli}.
   *
   * <p>Usually one element — the bleep binary. But a bleep started on a JVM runs as {@code java -cp
   * <classpath> bleep.Main}, and then the invocation is several elements of which the first is
   * {@code java}. Prefer this over {@link #bleepExecutable()}, which cannot represent that case.
   *
   * <pre>{@code
   * Cli.command("bleep").args(started.bleepCommand()).args("compile").run(started);
   * }</pre>
   */
  List<String> bleepCommand();

  /**
   * This bleep as a single path.
   *
   * <p>Throws when the invocation is not a bare binary — see {@link #bleepCommand()}, which always
   * works and is what you want unless you specifically need a file on disk.
   */
  Path bleepExecutable();
}
