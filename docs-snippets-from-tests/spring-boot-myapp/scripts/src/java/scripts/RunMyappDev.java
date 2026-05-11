package scripts;

import bleep.plugin.springboot.SpringBootRun;
import bleepscript.BleepScript;
import bleepscript.Commands;
import bleepscript.Started;
import java.util.List;

/** Run myapp in dev mode: dev profile, smaller heap, live resource edits. */
public class RunMyappDev extends BleepScript {
  public RunMyappDev() {
    super("run-myapp-dev");
  }

  @Override
  public void run(Started started, Commands commands, List<String> args) {
    new SpringBootRun()
        .withJvmArgs("-Xmx512m")
        .withProfiles("dev")
        .withAddResources(true)
        .withEnvironment("APP_PORT", "9090")
        .runOn(started, commands, "myapp");
  }
}
