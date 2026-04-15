package bleepscript.examples;

import bleepscript.BleepScript;
import bleepscript.Commands;
import bleepscript.Started;
import java.util.List;

/**
 * The smallest possible bleep script written in Java.
 *
 * <p>Run with {@code bleep hello}. Every Java script follows the same three-step pattern:
 *
 * <ol>
 *   <li>Extend {@link BleepScript} and pass a name to the super constructor.
 *   <li>Implement {@link #run} — this is where your logic goes.
 *   <li>Write a tiny {@code main} that calls {@code bootstrap(args)}.
 * </ol>
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

  public static void main(String[] args) {
    new Hello().bootstrap(args);
  }
}
