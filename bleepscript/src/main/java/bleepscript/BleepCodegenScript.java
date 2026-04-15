package bleepscript;

import java.util.List;
import java.util.Objects;

/**
 * Base class for user-authored codegen scripts. Unlike {@link BleepScript}, the framework syncs
 * generated files from a temp directory to the real generated-sources/resources directory after
 * {@link #run} returns successfully, preserving timestamps of unchanged files and writing a stamp
 * file for staleness detection.
 */
public abstract class BleepCodegenScript {
  private final String name;

  protected BleepCodegenScript(String name) {
    this.name = Objects.requireNonNull(name, "name");
  }

  public final String scriptName() {
    return name;
  }

  /** The class name used to namespace generated-sources directories. */
  public final String thisClassName() {
    return getClass().getName().split("\\$")[0];
  }

  public abstract void run(
      Started started, Commands commands, List<CodegenTarget> targets, List<String> args);

  public final void bootstrap(String[] args) {
    BleepscriptServices.Holder.INSTANCE.forCodegen(name, thisClassName(), args, this);
  }
}
