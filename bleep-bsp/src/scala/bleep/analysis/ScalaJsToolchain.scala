package bleep.analysis

import cats.effect.IO
import java.nio.file.Path

/** Trait for Scala.js linking toolchains.
  *
  * Provides an abstraction over different Scala.js linker versions.
  */
trait ScalaJsToolchain {

  /** Link Scala.js IR to JavaScript.
    *
    * @param config
    *   the linker configuration
    * @param classpath
    *   the classpath containing Scala.js IR files (.sjsir)
    * @param mainClass
    *   optional main class to run
    * @param outputDir
    *   the directory to output JS files
    * @param moduleName
    *   the name of the output module
    * @param logger
    *   a logger for linker output
    * @param cancellation
    *   token for cancelling linking
    * @param isTest
    *   true if linking for tests (uses test module initializers when no mainClass)
    * @return
    *   the result of linking, or IO.canceled if cancelled
    */
  def link(
      config: ScalaJsLinkConfig,
      classpath: Seq[Path],
      mainClass: Option[String],
      outputDir: Path,
      moduleName: String,
      logger: ScalaJsToolchain.Logger,
      cancellation: CancellationToken,
      isTest: Boolean = false
  ): IO[ScalaJsLinkResult]
}

object ScalaJsToolchain {

  /** Logger interface for linker output.
    */
  trait Logger {
    def trace(message: => String): Unit
    def debug(message: => String): Unit
    def info(message: => String): Unit
    def warn(message: => String): Unit
    def error(message: => String): Unit
  }

  object Logger {
    val Silent: Logger = new Logger {
      def trace(message: => String): Unit = ()
      def debug(message: => String): Unit = ()
      def info(message: => String): Unit = ()
      def warn(message: => String): Unit = ()
      def error(message: => String): Unit = ()
    }
  }

  /** Create a Scala.js toolchain for the specified version.
    *
    * @param scalaJsVersion
    *   the Scala.js version (e.g., "1.16.0", "1.19.0")
    * @param scalaVersion
    *   the Scala version (e.g., "3.3.3", "2.13.15")
    * @return
    *   a toolchain instance
    */
  def forVersion(scalaJsVersion: String, scalaVersion: String): ScalaJsToolchain = {
    val majorVersion = scalaJsVersion.split('.').headOption.getOrElse("1")
    majorVersion match {
      case "1"   => new ScalaJs1Bridge(scalaJsVersion, scalaVersion)
      case other =>
        throw new IllegalArgumentException(s"Unsupported Scala.js version: $scalaJsVersion (major version $other)")
    }
  }
}
