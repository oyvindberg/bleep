package bleepscript;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Forks and waits on an external process, streaming stdout/stderr through the build's logger.
 * Throws on non-zero exit code with the action, cwd, env, and exit code captured for diagnostics.
 *
 * <p>Prefer this over raw {@link ProcessBuilder} from a bleep script: it picks up bleep's {@code
 * arch -arch arm64} unjailing on Apple Silicon, integrates with the structured logger, and produces
 * a uniform error shape across the build.
 *
 * <pre>{@code
 * Cli.command("proguard")
 *    .args(jvm, "-cp", classpath, "proguard.ProGuard", "@" + configFile)
 *    .cwd(workdir)
 *    .run(started);
 * }</pre>
 */
public final class Cli {
  private Cli() {}

  /**
   * Start building a command. {@code action} is a short label used in log lines and the exception
   * message on failure.
   */
  public static Builder command(String action) {
    return new Builder(action);
  }

  public static final class Builder {
    private final String action;
    private List<String> command = List.of();
    private Path cwd;
    private final List<Map.Entry<String, String>> env = new ArrayList<>();

    private Builder(String action) {
      this.action = Objects.requireNonNull(action, "action");
    }

    public Builder args(String... cmd) {
      this.command = List.of(cmd);
      return this;
    }

    public Builder args(List<String> cmd) {
      this.command = List.copyOf(cmd);
      return this;
    }

    /**
     * Working directory for the child process. Required: bleep's process model always sets cwd
     * explicitly to keep behavior portable.
     */
    public Builder cwd(Path cwd) {
      this.cwd = Objects.requireNonNull(cwd, "cwd");
      return this;
    }

    /** Add an environment variable. Repeat to add more; later additions override earlier ones. */
    public Builder env(String key, String value) {
      env.add(
          Map.entry(Objects.requireNonNull(key, "key"), Objects.requireNonNull(value, "value")));
      return this;
    }

    /**
     * Fork, wait, stream output. Throws if the process exits non-zero. Returns a record with the
     * captured stdout / stderr lines on success.
     */
    public Result run(Started started) {
      Objects.requireNonNull(started, "started");
      if (command.isEmpty()) throw new IllegalStateException("command is required");
      if (cwd == null) throw new IllegalStateException("cwd is required");
      return BleepscriptServices.Holder.INSTANCE.runCli(started, action, command, cwd, env);
    }
  }

  /** Result of a successful command. */
  public record Result(List<String> stdout, List<String> stderr) {
    public Result {
      Objects.requireNonNull(stdout, "stdout");
      Objects.requireNonNull(stderr, "stderr");
      stdout = List.copyOf(stdout);
      stderr = List.copyOf(stderr);
    }
  }
}
