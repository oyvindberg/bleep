package bleep

import coursier.cache.FileCache
import coursier.util.Task

/** Bleep-wide [[FileCache]] factory.
  *
  * Coursier's [[FileCache]] in this version (`io.get-coursier:coursier-cache_2.13:2.1.24`) exposes no API for read/connect timeouts. The underlying
  * `HttpURLConnection` therefore uses the JVM defaults, which are **infinite** unless overridden by the `sun.net.client.defaultConnectTimeout` /
  * `defaultReadTimeout` system properties.
  *
  * Without those overrides, a stalled HTTPS read in coursier (e.g. a redirect chase that lands on a slow server) blocks the coursier-pool thread forever, which
  * in turn blocks the calling code via the `Await.result(..., Inf)` patterns in [[bleep.FetchJvm]] and friends. Surfaced on GHA as the IT-suite hang in #580 —
  * a coursier-pool thread sat in `sun.security.ssl.SSLSocketImpl.read` for the entire 120 s suite-idle window.
  *
  * [[ensureSystemPropertyDefaults]] sets the two `sun.net.client.*` properties if they aren't already configured. We call it from this object's static
  * initializer so any [[apply]]/[[at]] consumer also gets the timeouts in effect. Anyone else creating their own HttpURLConnection inherits the same defaults
  * from here on.
  */
object BleepFileCache {
  import scala.concurrent.duration.*

  /** Time to wait while opening a TCP connection. */
  val connectTimeoutMs: Long = 10.seconds.toMillis

  /** Per-read socket timeout — fires only on stalled connections, not on slow but progressing downloads. */
  val readTimeoutMs: Long = 30.seconds.toMillis

  ensureSystemPropertyDefaults()

  /** Idempotent. Sets the JVM-wide HTTP/HTTPS connect/read timeouts unless the user already set them. */
  def ensureSystemPropertyDefaults(): Unit = {
    if (System.getProperty("sun.net.client.defaultConnectTimeout") == null)
      System.setProperty("sun.net.client.defaultConnectTimeout", connectTimeoutMs.toString)
    if (System.getProperty("sun.net.client.defaultReadTimeout") == null)
      System.setProperty("sun.net.client.defaultReadTimeout", readTimeoutMs.toString)
  }

  def apply(): FileCache[Task] = FileCache[Task]()

  def at(location: java.io.File): FileCache[Task] = FileCache[Task](location)
}
