package bleepscript;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

/**
 * The bleep version this bleepscript jar was built as, used by {@link
 * BleepscriptServices.Holder#bootstrap} to fetch the matching bleep-core_3 artifact when no
 * implementation is found on the classpath.
 *
 * <p>Read from bleepscript's {@code dynver} stamp ({@code stamp: dynver} in bleep's own build), not
 * generated into a constant. The constant was bleep's own instance of issue #669: a value derived
 * from git is not an input its generator watches, so it went stale; it changed on every commit, so
 * bleepscript and everything downstream of it — bleep-core, and so nearly the whole build — missed
 * the remote cache on every commit; and {@code bleep publish --version X} shipped a jar whose
 * constant still said the dynver value. A stamp is recomputed every build, sits in no cache key,
 * and a publish stamps the version it publishes under.
 */
public final class BleepscriptVersion {
  private BleepscriptVersion() {}

  private static final String RESOURCE = "/bleep-stamp/bleepscript.properties";

  /**
   * Read on each call rather than in a static initializer: a missing stamp then fails with this
   * message wherever the version is needed, instead of an {@code ExceptionInInitializerError} once
   * and a bare {@code NoClassDefFoundError} on every access after.
   */
  public static String value() {
    try (InputStream in = BleepscriptVersion.class.getResourceAsStream(RESOURCE)) {
      if (in == null)
        throw new IllegalStateException(
            "bleepscript cannot tell its own version: "
                + RESOURCE
                + " is not on the classpath. Either this jar was built by a bleep that does not"
                + " write stamps (bleepscript declares `stamp: dynver`, which needs a bleep new"
                + " enough to honour it), or it is running from compiled classes without their"
                + " generated resources.");
      Properties props = new Properties();
      props.load(in);
      String version = props.getProperty("dynver");
      if (version == null)
        throw new IllegalStateException(
            RESOURCE + " has no `dynver` entry: " + props.stringPropertyNames());
      return version;
    } catch (IOException e) {
      throw new IllegalStateException("could not read " + RESOURCE, e);
    }
  }
}
