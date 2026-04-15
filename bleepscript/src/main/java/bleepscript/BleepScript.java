package bleepscript;

import java.util.List;
import java.util.Objects;

/**
 * Base class for user-authored bleep scripts written in Java.
 *
 * <p>Typical usage:
 *
 * <pre>{@code
 * public final class MyScript extends BleepScript {
 *   public MyScript() { super("my-script"); }
 *
 *   @Override
 *   public void run(Started started, Commands commands, List<String> args) {
 *     started.logger().info("hello");
 *     commands.compile(List.of(CrossProjectName.of("my-project")));
 *   }
 * }
 * }</pre>
 *
 * <p>No explicit {@code main} method is required — the inherited {@link #main(String[])} below
 * discovers the launched class via {@code sun.java.command} and instantiates it using a no-arg
 * constructor.
 */
public abstract class BleepScript {
  private final String name;

  protected BleepScript(String name) {
    this.name = Objects.requireNonNull(name, "name");
  }

  public final String scriptName() {
    return name;
  }

  public abstract void run(Started started, Commands commands, List<String> args);

  public final void bootstrap(String[] args) {
    BleepscriptServices.Holder.INSTANCE.forScript(name, args, this);
  }

  /**
   * JVM entry point inherited by every subclass. When the JVM launches {@code java MyScript ...},
   * this method reads the main class name from {@code sun.java.command}, instantiates it via its
   * no-arg constructor, and calls {@link #bootstrap(String[])}.
   */
  public static void main(String[] args) throws ReflectiveOperationException {
    BleepScript script = Launcher.instantiate(BleepScript.class);
    script.bootstrap(args);
  }
}
