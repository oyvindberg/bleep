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

  /** Default max heap for the BSP server: a quarter of physical RAM, clamped to [4g, 16g].
    *
    * The old fixed 4g cap OOM-killed the server repeatedly on large builds under concurrent clients, even on machines with plenty of RAM. Machine-dependent but
    * deterministic per machine, so it is safe to include in the JvmKey hash. Overridable via `bleep config compile-server max-memory`.
    */
  val defaultMaxHeapMb: Long = {
    val physicalMb = bleep.MachineResources.physicalMemoryMb(fallbackMb = 16 * 1024L)
    math.min(16 * 1024L, math.max(4 * 1024L, physicalMb / 4))
  }

  def defaultMaxHeapOpt: String = s"-Xmx${defaultMaxHeapMb}m"

  /** Default JVM options for BSP server. Does NOT include -Xmx: the heap cap comes from user config or [[defaultMaxHeapOpt]] (see SetupBleepBsp). */
  val defaultJavaOpts: Seq[String] = Seq(
    "-Xss4m",
    "-XX:ReservedCodeCacheSize=512m",
    "-XX:+UseZGC",
    "-XX:+ZGenerational",
    "-XX:ZUncommitDelay=30",
    "-XX:ZCollectionInterval=5",
    "-XX:+UseStringDeduplication",
    // No OS core file on a hard VM crash: multi-GB, one per crash, and nothing ever reads them.
    "-XX:-CreateCoredumpOnCrash",
    // Route VM tty output to stderr, which startServer captures into the socket dir's `output`
    // log (stdout is discarded). Notably this captures "Terminating due to
    // java.lang.OutOfMemoryError" from ExitOnOutOfMemoryError below — the marker clients use to
    // diagnose OOM death (BspServerOperations.OomMarker). We deliberately do NOT
    // -XX:+HeapDumpOnOutOfMemoryError: dumps are ~heap-sized, accumulate one per crash, and
    // freeze the JVM for minutes mid-write, during which clients hang on a dead-but-alive server.
    "-XX:+DisplayVMOutputToStderr",
    // Die immediately on OOM instead of limping on. A worker-thread OOM otherwise poisons
    // static initializers (e.g. sbt.nio.file.FileTreeView$ fails init once, then throws
    // NoClassDefFoundError forever), silently bricking all further compiles for the server's
    // lifetime. The OOM is typically a transient allocation spike (retained heap is far below
    // -Xmx), so a clean exit + BspRifle restart recovers, where continuing does not.
    "-XX:+ExitOnOutOfMemoryError",
    "-XX:+IgnoreUnrecognizedVMOptions"
  )

  /** JVM options that require a minimum JDK version */
  def jdkVersionOpts(jvmMajorVersion: Int): Seq[String] = {
    val opts = Seq.newBuilder[String]
    if (jvmMajorVersion >= 25) {
      opts += "-XX:+UseCompactObjectHeaders"
    }
    opts.result()
  }

  /** Extract major JDK version from a JVM name like "graalvm-community:25.0.1" or "temurin:21.0.3". Returns 0 if the version cannot be parsed.
    */
  def jvmMajorVersion(jvmName: String): Int = {
    val versionPart = jvmName.lastIndexOf(':') match {
      case -1  => jvmName
      case idx => jvmName.substring(idx + 1)
    }
    val majorStr = versionPart.takeWhile(_.isDigit)
    if (majorStr.nonEmpty) majorStr.toInt else 0
  }

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
      javaOpts = defaultJavaOpts :+ defaultMaxHeapOpt,
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

/** Exception thrown when server is already running (exit code 222) */
class ServerAlreadyRunningException(socketDir: Path) extends RuntimeException(s"BSP server already running in $socketDir")
