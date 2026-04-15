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
 *
 *   public static void main(String[] args) {
 *     new MyScript().bootstrap(args);
 *   }
 * }
 * }</pre>
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
}
