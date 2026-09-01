package bleepscript;

import java.util.Objects;

/**
 * Where a publish gets its version: you say it, or bleep derives it from git.
 *
 * <p>One value replacing two fields. {@link PublishOptions} used to carry {@code versionOverride}
 * beside {@code versionFallback}, a {@code Supplier<String>} called only when the first was absent
 * — so "exactly one of these must be set" was a rule checked at construction and thrown as an
 * {@link IllegalArgumentException}, rather than something the type made impossible to get wrong.
 * The laziness that motivated the supplier belongs to bleep, which now decides whether to consult
 * git.
 */
public sealed interface PublishVersion permits PublishVersion.Specified, PublishVersion.Dynver {

  /** Publish exactly this version. */
  record Specified(String value) implements PublishVersion {
    public Specified {
      Objects.requireNonNull(value, "value");
      if (value.isEmpty()) throw new IllegalArgumentException("version must not be empty");
    }
  }

  /**
   * Derive the version from git tags: {@code <tag>} on a clean tag, {@code
   * <tag>+<distance>-<sha>[-SNAPSHOT]} otherwise. Git is consulted when the publish runs, not when
   * these options are built.
   */
  final class Dynver implements PublishVersion {
    public static final Dynver INSTANCE = new Dynver();

    private Dynver() {}
  }

  static PublishVersion of(String version) {
    return new Specified(version);
  }

  static PublishVersion fromGit() {
    return Dynver.INSTANCE;
  }
}
