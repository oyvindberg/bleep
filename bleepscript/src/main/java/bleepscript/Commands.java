package bleepscript;

import java.util.List;
import java.util.Optional;

public interface Commands {
  void compile(List<CrossProjectName> projects);

  void compile(List<CrossProjectName> projects, boolean watch);

  void test(List<CrossProjectName> projects);

  void test(
      List<CrossProjectName> projects,
      boolean watch,
      Optional<List<String>> only,
      Optional<List<String>> exclude);

  void run(CrossProjectName project);

  void run(
      CrossProjectName project,
      Optional<String> overrideMainClass,
      List<String> args,
      boolean raw,
      boolean watch);

  void clean(List<CrossProjectName> projects);

  void script(String scriptName, List<String> args);

  void script(String scriptName, List<String> args, boolean watch);

  void publishLocal(PublishOptions options);

  void publishLocal(PublishOptions options, boolean watch);
}
