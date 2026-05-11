package scripts;

import bleep.plugin.springboot.SpringBootRun;
import bleepscript.BleepScript;
import bleepscript.Commands;
import bleepscript.Started;
import java.util.List;

/** Run myapp in prod mode: prod profile, larger heap, G1GC tuning. */
public class RunMyappProd extends BleepScript {
  public RunMyappProd() {
    super("run-myapp-prod");
  }

  @Override
  public void run(Started started, Commands commands, List<String> args) {
    new SpringBootRun()
        .withJvmArgs("-Xmx2g", "-XX:+UseG1GC", "-XX:MaxGCPauseMillis=200")
        .withProfiles("prod")
        .withEnvironment("APP_PORT", "9091")
        .runOn(started, commands, "myapp");
  }
}
