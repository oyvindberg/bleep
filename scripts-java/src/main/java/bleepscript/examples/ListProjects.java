package bleepscript.examples;

import bleepscript.BleepScript;
import bleepscript.Commands;
import bleepscript.CrossProjectName;
import bleepscript.Logger;
import bleepscript.Project;
import bleepscript.Started;
import java.util.List;
import java.util.Map;

/**
 * Walks every project in the build and prints a one-line summary.
 *
 * <p>Demonstrates reading {@link bleepscript.Build#explodedProjects()} and per-project fields like
 * dependency count, test-project flag, and platform kind.
 *
 * <p>Run with {@code bleep list-projects}.
 */
public final class ListProjects extends BleepScript {
  public ListProjects() {
    super("list-projects");
  }

  @Override
  public void run(Started started, Commands commands, List<String> args) {
    Logger logger = started.logger();
    Map<CrossProjectName, Project> projects = started.build().explodedProjects();

    logger.info("Build has " + projects.size() + " project(s):");
    projects.forEach(
        (name, project) -> {
          String platform = project.platform().map(p -> p.getClass().getSimpleName()).orElse("?");
          logger
              .withContext("project", name.asString())
              .info(
                  project.dependencies().size()
                      + " deps, "
                      + (project.isTestProject() ? "test" : "library")
                      + ", platform="
                      + platform);
        });
  }

  public static void main(String[] args) {
    new ListProjects().bootstrap(args);
  }
}
