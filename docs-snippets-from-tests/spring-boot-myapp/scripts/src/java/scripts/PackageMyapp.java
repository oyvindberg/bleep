package scripts;

import bleep.plugin.springboot.SpringBootRepackage;
import bleepscript.BleepScript;
import bleepscript.Commands;
import bleepscript.Started;
import java.util.List;

/** Build an executable fat JAR with layered Docker support. */
public class PackageMyapp extends BleepScript {
  public PackageMyapp() {
    super("package-myapp");
  }

  @Override
  public void run(Started started, Commands commands, List<String> args) {
    new SpringBootRepackage().withLayered(true).repackageOn(started, commands, "myapp");
  }
}
