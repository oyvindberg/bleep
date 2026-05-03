package bleepscript;

import coursierapi.Dependency;
import coursierapi.Fetch;
import coursierapi.Module;
import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.List;
import java.util.Optional;
import java.util.ServiceLoader;

/**
 * Service Provider Interface. The implementation lives in bleep-core (Scala) under {@code
 * META-INF/services/bleepscript.BleepscriptServices}. {@link Holder} loads it lazily — first via
 * {@link ServiceLoader} (used when bleep-core is already on the classpath, e.g. inside bleep
 * itself), and falling back to a coursier-driven bootstrap that downloads {@code bleep-core_3} +
 * its transitive deps and classloads them into a child class loader.
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
      // Fast path: impl already on classpath (true inside bleep itself, where bleep-core is a
      // regular dependency).
      Optional<BleepscriptServices> direct =
          ServiceLoader.load(BleepscriptServices.class).findFirst();
      if (direct.isPresent()) return direct.get();

      // Standalone path: download bleep-core_3 and its transitive deps via coursier, classload
      // them in a child loader, look up the impl there.
      return bootstrap();
    }

    private static BleepscriptServices bootstrap() {
      String version = resolveBleepscriptVersion();
      List<File> jars;
      try {
        jars =
            Fetch.create()
                .addDependencies(Dependency.of(Module.of("build.bleep", "bleep-core_3"), version))
                .fetch();
      } catch (coursierapi.error.CoursierError e) {
        throw new IllegalStateException(
            "Failed to fetch bleep-core_3:"
                + version
                + " via coursier. If you're offline, ensure the artifacts are in the coursier"
                + " cache.",
            e);
      }

      URL[] urls = jars.stream().map(BleepscriptServices.Holder::toUrl).toArray(URL[]::new);
      URLClassLoader loader = new URLClassLoader(urls, BleepscriptServices.class.getClassLoader());

      return ServiceLoader.load(BleepscriptServices.class, loader)
          .findFirst()
          .orElseThrow(
              () ->
                  new IllegalStateException(
                      "Bootstrapped bleep-core_3:"
                          + version
                          + " but ServiceLoader still couldn't find a"
                          + " bleepscript.BleepscriptServices implementation. The downloaded"
                          + " bleep-core artifact may be broken."));
    }

    /**
     * Pick the bleepscript version to fetch a matching bleep-core for. {@link
     * BleepscriptVersion#VALUE} is sourcegen'd into this jar at publish time from the same dynver
     * call that stamps bleep-core, so the two stay locked in sync. The system property is an escape
     * hatch for tests / bisecting.
     */
    private static String resolveBleepscriptVersion() {
      String sys = System.getProperty("bleepscript.version");
      if (sys != null && !sys.isBlank()) return sys;
      return BleepscriptVersion.VALUE;
    }

    private static URL toUrl(File f) {
      try {
        return f.toURI().toURL();
      } catch (Exception e) {
        throw new IllegalStateException("Could not convert " + f + " to URL", e);
      }
    }
  }
}
