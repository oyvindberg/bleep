package bleepscript;

import java.nio.file.Path;
import java.util.List;
import java.util.Objects;

/**
 * Fetches dependency classpaths through bleep's configured Coursier resolver. Uses the same cache,
 * repositories, authentication, and credential lookup that bleep itself uses when resolving project
 * dependencies.
 *
 * <p>Prefer this over {@code coursierapi.Fetch} directly from a script: it inherits the build's
 * resolver setup so private repos and offline behavior match what {@code bleep compile} does.
 *
 * <pre>{@code
 * List<Path> jars = Coursier.fetchClasspath(started, "com.guardsquare:proguard-base:7.5.0");
 * }</pre>
 */
public final class Coursier {
  private Coursier() {}

  /**
   * Resolve and download a dependency plus its transitive closure. Returns the local file paths,
   * suitable for use on a {@code -cp} flag.
   *
   * @param coordinates a dependency coordinate string. Java artifacts use a single colon ( {@code
   *     org:name:version}); Scala-cross-versioned artifacts use a double colon ( {@code
   *     org::name:version}).
   */
  public static List<Path> fetchClasspath(Started started, String coordinates) {
    Objects.requireNonNull(started, "started");
    Objects.requireNonNull(coordinates, "coordinates");
    return BleepscriptServices.Holder.INSTANCE.fetchClasspath(started, coordinates);
  }
}
