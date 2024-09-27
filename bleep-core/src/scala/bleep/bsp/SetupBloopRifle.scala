package bleep
package bsp

import bleep.internal.FileUtils
import bleep.logging.Logger
import bloop.rifle.internal.BuildInfo
import bloop.rifle.{BloopRifleConfig, BloopVersion}

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.*
import java.nio.file.attribute.PosixFilePermission
import scala.collection.immutable.SortedSet
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Properties, Random, Success, Try}

object SetupBloopRifle {
  def apply(
      compileServerMode: model.CompileServerMode,
      resolvedJvm: ResolvedJvm,
      userPaths: UserPaths,
      resolver: CoursierResolver,
      bleepRifleLogger: BleepRifleLogger
  ): BloopRifleConfig =
    BloopRifleConfig(
      address = BloopRifleConfig.Address.DomainSocket(bspSocketFile(bleepRifleLogger.logger, userPaths, compileServerMode, resolvedJvm)),
      javaPath = resolvedJvm.javaBin.toString,
      javaOpts = mkJavacOptionsFor(resolvedJvm),
      classPath = mkBloopClassPath(resolver),
      workingDir = FileUtils.TempDir.toFile,
      bspSocketOrPort = None,
      bspStdin = None,
      bspStdout = bleepRifleLogger.bloopBspStdout,
      bspStderr = bleepRifleLogger.bloopBspStderr,
      period = 10.millis,
      timeout = 10.seconds + BloopRifleConfig.extraTimeout,
      startCheckPeriod = 100.millis,
      startCheckTimeout = 1.minute + BloopRifleConfig.extraTimeout,
      initTimeout = 30.seconds + BloopRifleConfig.extraTimeout,
      minimumBloopJvm = 17,
      retainedBloopVersion = BloopRifleConfig.AtLeast(BloopVersion(BuildInfo.version))
    )

  def mkJavacOptionsFor(resolvedJvm: ResolvedJvm): Seq[String] =
    resolvedJvm.jvm.compileServerOptionIncludeDefault match {
      case Some(false) => resolvedJvm.jvm.compileServerOptions.render
      case _           => BloopRifleConfig.hardCodedDefaultJavaOpts ++ resolvedJvm.jvm.compileServerOptions.renderCombinedArgs
    }

  val parallelCollectionAlways = model.LibraryVersionScheme(
    model.LibraryVersionScheme.VersionScheme.Always,
    model.Dep.Scala("org.scala-lang.modules", "scala-parallel-collections", "always")
  )
  val versionCombo = model.VersionCombo.Jvm(model.VersionScala.Scala212)

  def mkBloopClassPath(resolver: CoursierResolver)(bloopVersion: String): Either[BleepException, Seq[File]] = {
    val dep = model.Dep.Scala("ch.epfl.scala", "bloop-frontend", bloopVersion)
    resolver
      .updatedParams(_.copy(downloadSources = false))
      .resolve(Set(dep), versionCombo, libraryVersionSchemes = SortedSet(parallelCollectionAlways)) match {
      case Left(coursierError) =>
        Left(new BleepException.ResolveError(coursierError, "installing bloop"))
      case Right(value) =>
        Right(value.jarFiles)
    }
  }

  private def socketDirectory(userPaths: UserPaths, socketId: String): Path = {
    val dir = userPaths.bspSocketDir
    // Ensuring that whenever dir exists, it has the right permissions
    if (!Files.isDirectory(dir)) {
      val tmpDir = dir.getParent / s".${dir.getFileName}.tmp-$socketId"
      try {
        Files.createDirectories(tmpDir)
        if (!Properties.isWin) {
          Files.setPosixFilePermissions(
            tmpDir,
            java.util.Set.of(PosixFilePermission.OWNER_EXECUTE, PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE)
          )
          ()
        }
        try {
          Files.move(tmpDir, dir, StandardCopyOption.ATOMIC_MOVE)
          ()
        } catch {
          case _: AtomicMoveNotSupportedException =>
            try {
              Files.move(tmpDir, dir)
              ()
            } catch {
              case _: FileAlreadyExistsException =>
            }
          case _: FileAlreadyExistsException =>
        }
      } finally {
        Files.deleteIfExists(tmpDir)
        ()
      }
    }
    dir
  }
  def bspSocketFile(logger: Logger, userPaths: UserPaths, mode: model.CompileServerMode, jvm: ResolvedJvm): Path = {
    val somewhatRandomIdentifier = Try(ProcessHandle.current.pid) match {
      case Failure(_) =>
        val r = new Random
        s"conn-${r.nextInt()}"
      case Success(pid) => s"proc-$pid"
    }

    val socketName: String =
      mode match {
        case model.CompileServerMode.NewEachInvocation => somewhatRandomIdentifier
        case model.CompileServerMode.Shared            =>
          // windows does not accept colon in path names
          val name = jvm.jvm.name.replace(':', '_')
          val optionDigest = Checksums.compute(mkJavacOptionsFor(jvm).mkString("").getBytes(StandardCharsets.UTF_8), Checksums.Algorithm.Md5)
          name + "_" + optionDigest.hexString.take(8)
      }

    val socket: Path =
      socketDirectory(userPaths, somewhatRandomIdentifier) / socketName

    mode match {
      case model.CompileServerMode.NewEachInvocation =>
        Runtime.getRuntime.addShutdownHook(
          new Thread("delete-bloop-bsp-named-socket") {
            override def run(): Unit =
              try FileUtils.deleteDirectory(socket)
              catch {
                case x: FileSystemException => logger.warn(s"Failed to delete $socket at shutdown: ${x.getMessage}")
              }
          }
        )

      case model.CompileServerMode.Shared => ()
    }

    socket
  }
}
