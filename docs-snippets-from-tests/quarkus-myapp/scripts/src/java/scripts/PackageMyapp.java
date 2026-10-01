package scripts;

import bleep.plugin.quarkus.QuarkusPackage;
import bleepscript.BleepScript;
import bleepscript.Commands;
import bleepscript.Started;
import java.util.List;

/** Production build: writes the fast-jar quarkus-app/ layout plus quarkus-artifact.properties. */
public class PackageMyapp extends BleepScript {
  public PackageMyapp() {
    super("package-myapp");
  }

  @Override
  public void run(Started started, Commands commands, List<String> args) {
    new QuarkusPackage().packageOn(started, commands, "myapp");
  }
}
