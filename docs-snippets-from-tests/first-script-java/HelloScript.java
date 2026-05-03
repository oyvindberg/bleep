package scripts;

import bleepscript.BleepScript;
import bleepscript.Commands;
import bleepscript.Started;
import java.util.List;

public final class HelloScript extends BleepScript {
  public HelloScript() {
    super("hello");
  }

  @Override
  public void run(Started started, Commands commands, List<String> args) {
    int projectCount = started.build().explodedProjects().size();
    started.logger().info("This build has " + projectCount + " projects");
    if (args.isEmpty()) {
      started.logger().info("Hello, world!");
    } else {
      started.logger().info("Hello, " + String.join(" ", args) + "!");
    }
  }
}
