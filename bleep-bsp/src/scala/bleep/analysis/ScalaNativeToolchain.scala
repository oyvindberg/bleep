package bleep.analysis

import cats.effect.IO
import java.nio.file.Path

/** Trait for Scala Native linking toolchains.
  *
  * Provides an abstraction over different Scala Native linker versions.
  */
trait ScalaNativeToolchain {

  /** Link Scala Native NIR to native binary.
    *
    * @param config
    *   the linker configuration
    * @param classpath
    *   the classpath containing Scala Native NIR files
    * @param mainClass
    *   the main class to run
    * @param outputPath
    *   the path for the output binary
    * @param workDir
    *   the working directory for linking
    * @param logger
    *   a logger for linker output
    * @param cancellation
    *   token for cancelling linking
    * @return
    *   the result of linking, or IO.canceled if cancelled
    */
  def link(
      config: ScalaNativeLinkConfig,
      classpath: Seq[Path],
      mainClass: String,
      outputPath: Path,
      workDir: Path,
      logger: ScalaNativeToolchain.Logger,
      cancellation: CancellationToken
  ): IO[ScalaNativeLinkResult]
}

object ScalaNativeToolchain {

  /** Logger interface for linker output.
    */
  trait Logger {
    def trace(message: => String): Unit
    def debug(message: => String): Unit
    def info(message: => String): Unit
    def warn(message: => String): Unit
    def error(message: => String): Unit
    def running(command: Seq[String]): Unit
  }

  object Logger {
    val Silent: Logger = new Logger {
      def trace(message: => String): Unit = ()
      def debug(message: => String): Unit = ()
      def info(message: => String): Unit = ()
      def warn(message: => String): Unit = ()
      def error(message: => String): Unit = ()
      def running(command: Seq[String]): Unit = ()
    }

  }

  /** Create a Scala Native toolchain for the specified version.
    *
    * @param scalaNativeVersion
    *   the Scala Native version (e.g., "0.4.17", "0.5.6")
    * @param scalaVersion
    *   the Scala version (e.g., "3.3.3", "2.13.15")
    * @return
    *   a toolchain instance
    */
  def forVersion(scalaNativeVersion: String, scalaVersion: String): ScalaNativeToolchain = {
    val parts = scalaNativeVersion.split('.')
    val majorMinor = if (parts.length >= 2) s"${parts(0)}.${parts(1)}" else scalaNativeVersion

    majorMinor match {
      case "0.4" => new ScalaNative04Bridge(scalaNativeVersion, scalaVersion)
      case "0.5" => new ScalaNative05Bridge(scalaNativeVersion, scalaVersion)
      case "1.0" => new ScalaNative05Bridge(scalaNativeVersion, scalaVersion) // 1.0 uses similar API to 0.5
      case other =>
        // Try to use 0.5 bridge for newer versions
        if (parts.headOption.exists(v => v.toIntOption.exists(_ >= 1))) {
          new ScalaNative05Bridge(scalaNativeVersion, scalaVersion)
        } else {
          throw new IllegalArgumentException(s"Unsupported Scala Native version: $scalaNativeVersion")
        }
    }
  }
}
