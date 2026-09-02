package bleepscript;

import java.util.Objects;
import java.util.Optional;

/**
 * What {@link Commands#link} passes to the linker — the same settings {@code bleep link} accepts as
 * flags.
 *
 * <p>Every field a platform does not have a use for is ignored by it: {@code moduleKind} and {@code
 * minify} are JavaScript's, {@code lto} and {@code debugInfo} are the native platforms'. An empty
 * {@link Optional} means "whatever the build and the mode decide", which is not the same as an
 * explicit {@code false}: a project declaring {@code jsKind: esmodule} keeps its declaration when
 * {@code moduleKind} is empty and loses it when it is set.
 *
 * <p>{@link #DEBUG} and {@link #RELEASE} are the two most callers want.
 */
public record LinkOptions(
    boolean releaseMode,
    Optional<Boolean> sourceMaps,
    Optional<Boolean> minify,
    Optional<ModuleKind> moduleKind,
    Optional<LTO> lto,
    Optional<Boolean> optimize,
    Optional<Boolean> debugInfo) {

  public LinkOptions {
    Objects.requireNonNull(sourceMaps, "sourceMaps");
    Objects.requireNonNull(minify, "minify");
    Objects.requireNonNull(moduleKind, "moduleKind");
    Objects.requireNonNull(lto, "lto");
    Objects.requireNonNull(optimize, "optimize");
    Objects.requireNonNull(debugInfo, "debugInfo");
  }

  /** Module kind for JavaScript output (Scala.js). */
  public enum ModuleKind {
    NO_MODULE,
    COMMON_JS,
    ES_MODULE
  }

  /** Link-time optimization level (Scala Native). */
  public enum LTO {
    NONE,
    THIN,
    FULL
  }

  /** Debug semantics, nothing overridden. What {@code bleep link} does with no flags. */
  public static final LinkOptions DEBUG =
      new LinkOptions(
          false,
          Optional.empty(),
          Optional.empty(),
          Optional.empty(),
          Optional.empty(),
          Optional.empty(),
          Optional.empty());

  /** Release semantics, nothing overridden. What {@code bleep link --release} does. */
  public static final LinkOptions RELEASE =
      new LinkOptions(
          true,
          Optional.empty(),
          Optional.empty(),
          Optional.empty(),
          Optional.empty(),
          Optional.empty(),
          Optional.empty());
}
