package bleepscript.examples;

import bleepscript.BleepScript;
import bleepscript.Commands;
import bleepscript.CrossProjectName;
import bleepscript.Dep;
import bleepscript.Logger;
import bleepscript.Project;
import bleepscript.Started;
import java.util.List;

/**
 * Prints the declared dependencies of a given project.
 *
 * <p>Reads {@code args[0]} as the project name (e.g. {@code bleep-core}) and iterates {@link
 * Project#dependencies()}. Demonstrates:
 *
 * <ul>
 *   <li>Passing arguments to a script.
 *   <li>Looking up a project from the build.
 *   <li>Working with the sealed {@link Dep} type (pattern matching with {@code instanceof}).
 * </ul>
 *
 * <p>Run with {@code bleep show-deps <project-name>}.
 */
public final class ShowDeps extends BleepScript {
  public ShowDeps() {
    super("show-deps");
  }

  @Override
  public void run(Started started, Commands commands, List<String> args) {
    Logger logger = started.logger();
    if (args.isEmpty()) {
      logger.error("usage: bleep show-deps <project-name>");
      return;
    }
    String wanted = args.get(0);

    started.build().explodedProjects().entrySet().stream()
        .filter(e -> e.getKey().name().equals(wanted))
        .forEach(
            entry -> {
              CrossProjectName cross = entry.getKey();
              Project project = entry.getValue();
              Logger scoped = logger.withContext("project", cross.asString());
              scoped.info(project.dependencies().size() + " dependencies:");
              project
                  .dependencies()
                  .forEach(
                      dep -> {
                        String kind =
                            switch (dep) {
                              case Dep.Java j -> "java";
                              case Dep.Scala s -> s.fullCrossVersion() ? "scala-full" : "scala";
                            };
                        scoped.info("  " + kind + ": " + dep.repr());
                      });
            });
  }
}
