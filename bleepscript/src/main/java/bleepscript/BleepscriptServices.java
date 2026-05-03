package bleepscript;

import java.util.ServiceLoader;

/**
 * Service Provider Interface. Implemented by bleep-core (in the javaapi package) and registered via
 * {@code META-INF/services}. Loaded lazily on first use.
 */
public interface BleepscriptServices {
  /** Bootstrap a regular script: loads build, creates Started/Commands, runs user's script. */
  void forScript(String scriptName, String[] args, BleepScript script);

  /**
   * Bootstrap a codegen script: wraps the existing Scala BleepCodegenScript machinery for temp-dir
   * sync and stamp files, then calls the user's run method with wrapped targets.
   */
  void forCodegen(
      String scriptName, String thisClassName, String[] args, BleepCodegenScript script);

  /** Parse a dependency coordinate string (throws IllegalArgumentException on bad input). */
  Dep parseDep(String coordinates);

  /** Parse a relative path (throws IllegalArgumentException on bad input). */
  RelPath parseRelPath(String path);

  /** Default ManifestCreator for publish-local. */
  ManifestCreator defaultManifestCreator();

  final class Holder {
    private Holder() {}

    public static final BleepscriptServices INSTANCE = load();

    private static BleepscriptServices load() {
      ServiceLoader<BleepscriptServices> loader = ServiceLoader.load(BleepscriptServices.class);
      return loader
          .findFirst()
          .orElseThrow(
              () ->
                  new IllegalStateException(
                      "No BleepscriptServices implementation found on classpath. "
                          + "Ensure bleep-core is on the runtime classpath."));
    }
  }
}
