package bleep.bsp

import java.nio.file.Path
import java.security.MessageDigest
import scala.concurrent.duration.*

/** JVM configuration key for sharing BSP servers.
  *
  * Multiple workspaces using the same JVM configuration can share a single BSP server instance. The key is hashed to create the socket directory name.
  */
case class JvmKey(
    bleepVersion: String,
    name: String,
    version: String,
    options: Seq[String]
) {

  /** Compute a stable hash for socket directory naming */
  def hash: String = {
    val content = s"$bleepVersion:$name:$version:${options.sorted.mkString(",")}"
    val digest = MessageDigest.getInstance("SHA-256")
    val bytes = digest.digest(content.getBytes("UTF-8"))
    bytes.take(8).map("%02x".format(_)).mkString
  }
}

/** Configuration for BSP server lifecycle management.
  *
  * Used by BspRifle to start, connect to, and manage BSP server processes. A single server can handle multiple workspaces if they share the same JVM key.
  */
case class BspRifleConfig(
    address: BspRifleConfig.Address,
    jvmKey: JvmKey,
    javaPath: Path,
    javaOpts: Seq[String],
    serverMainClass: String,
    serverClasspath: Seq[Path],
    workingDir: Path,
    startCheckPeriod: FiniteDuration,
    startCheckTimeout: FiniteDuration,
    connectionTimeout: FiniteDuration,
    shutdownGracePeriod: FiniteDuration,
    /** If true, server will exit when parent process dies (via stdin EOF detection). Used for ephemeral servers in NewEachInvocation mode.
      */
    dieWithParent: Boolean = false,
    /** Optional path for BSP JSON-RPC message tracing. If set, all messages are logged to this file. */
    traceFile: Option[Path] = None
)

object BspRifleConfig {

  /** Address type for BSP server communication */
  sealed trait Address {
    def socketDir: Path
  }

  object Address {

    /** Unix domain socket (preferred on Unix systems) */
    case class DomainSocket(path: Path) extends Address {
      def socketDir: Path = path.getParent
    }

    /** TCP socket (fallback for Windows or remote) */
    case class Tcp(host: String, port: Int, socketDir: Path) extends Address
  }

  /** Default JVM options for BSP server */
  val defaultJavaOpts: Seq[String] = Seq(
    "-Xss4m",
    "-Xmx4g",
    "-XX:+UseZGC",
    "-XX:ZUncommitDelay=30",
    "-XX:ZCollectionInterval=5",
    "-XX:+IgnoreUnrecognizedVMOptions"
  )

  /** Create a config with sensible defaults */
  def default(
      jvmKey: JvmKey,
      javaPath: Path,
      serverMainClass: String,
      serverClasspath: Seq[Path],
      workingDir: Path,
      userHome: Path
  ): BspRifleConfig = {
    val socketDir = socketDirForJvmKey(userHome, jvmKey)
    val socketPath = socketDir.resolve("socket")
    BspRifleConfig(
      address = Address.DomainSocket(socketPath),
      jvmKey = jvmKey,
      javaPath = javaPath,
      javaOpts = defaultJavaOpts,
      serverMainClass = serverMainClass,
      serverClasspath = serverClasspath,
      workingDir = workingDir,
      startCheckPeriod = 100.millis,
      startCheckTimeout = 1.minute,
      connectionTimeout = 10.seconds,
      shutdownGracePeriod = 5.seconds
    )
  }

  /** Derive socket directory from JVM key.
    *
    * Creates a unique directory under ~/.bleep/bsp/ based on JVM key hash. Multiple workspaces using the same JVM can share this socket.
    */
  def socketDirForJvmKey(userHome: Path, jvmKey: JvmKey): Path =
    userHome.resolve(".bleep").resolve("bsp").resolve(jvmKey.hash)

  /** Derive socket directory from workspace path (legacy, for single-workspace mode).
    *
    * Creates a unique directory under ~/.bleep/bsp/ based on workspace hash.
    */
  def socketDirForWorkspace(userHome: Path, workspaceDir: Path): Path = {
    val hash = workspaceDir.toAbsolutePath.normalize().toString.hashCode.toHexString
    userHome.resolve(".bleep").resolve("bsp").resolve(hash)
  }
}

/** Exception thrown when server fails to start within timeout */
class FailedToStartServerException(timeout: FiniteDuration) extends RuntimeException(s"BSP server failed to start within $timeout")

/** Exception thrown when server is already running (exit code 222) */
class ServerAlreadyRunningException(socketDir: Path) extends RuntimeException(s"BSP server already running in $socketDir")
