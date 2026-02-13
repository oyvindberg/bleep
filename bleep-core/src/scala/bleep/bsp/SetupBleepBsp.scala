package bleep.bsp

import bleep.*
import bleep.internal.FileUtils
import ryddig.Logger

import java.nio.file.*
import java.nio.file.attribute.PosixFilePermission
import scala.concurrent.duration.*
import scala.util.{Properties, Random, Try}

/** How the bleep-bsp server is provided to commands. */
sealed trait BspServerClasspathSource
object BspServerClasspathSource {

  /** In-process: instantiate the BSP server directly in the same JVM (used by integration tests).
    *
    * The connect function creates piped streams, starts the BSP server in a background thread, and returns a BspConnection.
    */
  case class InProcess(connect: ryddig.Logger => cats.effect.Resource[cats.effect.IO, BspConnection]) extends BspServerClasspathSource

  /** External process: resolve the published bleep-bsp artifact via Coursier, launch as a separate JVM. */
  case class FromCoursier(resolver: CoursierResolver) extends BspServerClasspathSource
}

/** Setup for bleep-bsp server configuration.
  *
  * Creates BspRifleConfig for connecting to the bleep-bsp compilation server.
  */
object SetupBleepBsp {

  def apply(
      compileServerMode: model.CompileServerMode,
      config: model.BleepConfig,
      resolvedJvm: ResolvedJvm,
      userPaths: UserPaths,
      resolver: CoursierResolver,
      logger: Logger,
      workingDir: Path
  ): Either[BleepException, BspRifleConfig] = {
    val extraJavaOpts = config.bspServerConfigOrDefault.compileServerMaxMemory.map(m => s"-Xmx$m").toList
    val majorVersion = BspRifleConfig.jvmMajorVersion(resolvedJvm.jvm.name)
    val versionOpts = BspRifleConfig.jdkVersionOpts(majorVersion)
    val javaOpts = BspRifleConfig.defaultJavaOpts ++ versionOpts ++ extraJavaOpts
    val jvmKey = createJvmKey(resolvedJvm.jvm, compileServerMode, versionOpts.toList ++ extraJavaOpts)

    for {
      serverClasspath <- resolveServerClasspath(resolver, userPaths, logger)
    } yield {
      val socketDir = socketDirectory(logger, userPaths, jvmKey, compileServerMode)
      val socketPath = socketDir.resolve("socket")

      // Ephemeral servers (NewEachInvocation) should die when parent dies
      val dieWithParent = compileServerMode == model.CompileServerMode.NewEachInvocation

      // BSP trace log for debugging JSON-RPC messages
      val traceFile = userPaths.cacheDir.resolve("bsp-trace.log")

      BspRifleConfig(
        address = BspRifleConfig.Address.DomainSocket(socketPath),
        jvmKey = jvmKey,
        javaPath = resolvedJvm.javaBin,
        javaOpts = javaOpts,
        serverMainClass = "bleep.bsp.BspServerDaemon",
        serverClasspath = serverClasspath,
        workingDir = workingDir,
        startCheckPeriod = 10.millis,
        startCheckTimeout = 1.minute,
        connectionTimeout = 10.seconds,
        shutdownGracePeriod = 5.seconds,
        dieWithParent = dieWithParent,
        traceFile = Some(traceFile)
      )
    }
  }

  /** Create a JVM key for socket sharing.
    *
    * In shared mode, the key is based on JVM name and version so different workspaces using the same JVM can share the server. In new-each-invocation mode,
    * include a unique identifier.
    */
  private def createJvmKey(jvm: model.Jvm, mode: model.CompileServerMode, extraJavaOpts: List[String]): JvmKey = {
    val bleepVersion = model.BleepVersion.current.value
    val allOptions = BspRifleConfig.defaultJavaOpts ++ extraJavaOpts

    mode match {
      case model.CompileServerMode.Shared =>
        JvmKey(
          bleepVersion = bleepVersion,
          name = jvm.name,
          version = jvm.index.getOrElse("default"),
          options = allOptions
        )
      case model.CompileServerMode.NewEachInvocation =>
        // Include a truly unique identifier for ephemeral servers (UUID ensures uniqueness even within same process)
        val uniqueId = java.util.UUID.randomUUID().toString.take(8)
        JvmKey(
          bleepVersion = bleepVersion,
          name = s"${jvm.name}-$uniqueId",
          version = jvm.index.getOrElse("default"),
          options = allOptions
        )
    }
  }

  private def classpathCacheFile(userPaths: UserPaths): Path =
    userPaths.cacheDir.resolve(s"bleep-bsp-classpath-${model.BleepVersion.current.value}.txt")

  private def resolveServerClasspath(resolver: CoursierResolver, userPaths: UserPaths, logger: Logger): Either[BleepException, Seq[Path]] = {
    val cacheFile = classpathCacheFile(userPaths)
    val cachedClasspath = Try {
      if (Files.exists(cacheFile)) {
        val paths = Files
          .readString(cacheFile)
          .split(java.io.File.pathSeparator)
          .filter(_.nonEmpty)
          .map(Paths.get(_))
          .toSeq
        if (paths.nonEmpty && Files.exists(paths.head)) Some(paths) else None
      } else None
    }.toOption.flatten

    cachedClasspath match {
      case Some(paths) =>
        logger.info(s"BSP server classpath from cache (${paths.size} entries)")
        Right(paths)
      case None =>
        val version = model.BleepVersion.current.value
        val dep = model.Dep.Scala("build.bleep", "bleep-bsp", version)
        val versionCombo = model.VersionCombo.Jvm(model.VersionScala.Scala3)
        logger.info(s"Resolving BSP server classpath for bleep-bsp:$version")

        resolver
          .updatedParams(_.copy(downloadSources = false))
          .resolve(
            Set(dep),
            versionCombo,
            libraryVersionSchemes = scala.collection.immutable.SortedSet.empty[model.LibraryVersionScheme],
            model.IgnoreEvictionErrors.No
          ) match {
          case Left(err) =>
            Left(new BleepException.ResolveError(err, s"installing bleep-bsp:$version"))
          case Right(resolved) =>
            val paths = resolved.jarFiles.map(_.toPath)
            logger.info(s"Resolved BSP server classpath: ${paths.size} jars")
            Try {
              Files.createDirectories(cacheFile.getParent)
              Files.writeString(cacheFile, paths.map(_.toString).mkString(java.io.File.pathSeparator))
            }
            Right(paths)
        }
    }
  }

  /** Create the socket directory with proper permissions.
    *
    * Uses JVM key hash for the directory name in shared mode.
    */
  private def socketDirectory(logger: Logger, userPaths: UserPaths, jvmKey: JvmKey, mode: model.CompileServerMode): Path = {
    val dir = userPaths.bspSocketDir.resolve(jvmKey.hash)

    // Ensure directory exists with proper permissions
    if (!Files.isDirectory(dir)) {
      val tmpId = Try(ProcessHandle.current.pid).getOrElse(Random.nextInt().toLong).toString
      val tmpDir = dir.getParent.resolve(s".${dir.getFileName}.tmp-$tmpId")

      try {
        Files.createDirectories(tmpDir)
        if (!Properties.isWin) {
          Files.setPosixFilePermissions(
            tmpDir,
            java.util.Set.of(
              PosixFilePermission.OWNER_EXECUTE,
              PosixFilePermission.OWNER_READ,
              PosixFilePermission.OWNER_WRITE
            )
          )
        }
        try
          Files.move(tmpDir, dir, StandardCopyOption.ATOMIC_MOVE)
        catch {
          case _: AtomicMoveNotSupportedException =>
            try Files.move(tmpDir, dir)
            catch { case _: FileAlreadyExistsException => () }
          case _: FileAlreadyExistsException => ()
        }
      } finally
        try Files.deleteIfExists(tmpDir)
        catch { case _: Exception => () }
    }

    // In NewEachInvocation mode, register cleanup on shutdown
    mode match {
      case model.CompileServerMode.NewEachInvocation =>
        Runtime.getRuntime.addShutdownHook(
          new Thread("delete-bleep-bsp-socket") {
            override def run(): Unit =
              try FileUtils.deleteDirectory(dir)
              catch {
                case x: java.nio.file.FileSystemException =>
                  logger.warn(s"Failed to delete $dir at shutdown: ${x.getMessage}")
              }
          }
        )
      case model.CompileServerMode.Shared => ()
    }

    dir
  }
}
