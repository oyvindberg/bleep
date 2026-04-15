package bleepscript.examples;

import bleepscript.BleepScript;
import bleepscript.Commands;
import bleepscript.Started;
import java.util.List;

/**
 * The smallest possible bleep script written in Java. Two steps:
 *
 * <ol>
 *   <li>Extend {@link BleepScript} and pass a name to the super constructor.
 *   <li>Implement {@link #run} — this is where your logic goes.
 * </ol>
 *
 * <p>Run with {@code bleep hello}.
 */
public final class Hello extends BleepScript {
  public Hello() {
    super("hello");
  }

  @Override
  public void run(Started started, Commands commands, List<String> args) {
    started.logger().info("Hello from a Java bleep script!");
    started.logger().info("Bleep version: " + started.build().version());
    if (!args.isEmpty()) {
      started.logger().info("You passed args: " + args);
    }
  }
}
