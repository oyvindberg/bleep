package scripts;

import bleep.plugin.quarkus.QuarkusRun;
import bleepscript.BleepScript;
import bleepscript.Commands;
import bleepscript.Started;
import java.util.List;

/** Run myapp in Quarkus dev mode: augmentation in-process, live reload from bleep's source dirs. */
public class RunMyappDev extends BleepScript {
  public RunMyappDev() {
    super("run-myapp-dev");
  }

  @Override
  public void run(Started started, Commands commands, List<String> args) {
    new QuarkusRun().withJvmArgs("-Xmx512m").runOn(started, commands, "myapp");
  }
}
