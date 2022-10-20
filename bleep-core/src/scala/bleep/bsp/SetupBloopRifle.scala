package bleep
package bsp

import bleep.internal.FileUtils
import bleep.logging.Logger
import coursier.parse.ModuleParser

import java.io.File
import java.nio.file._
import java.nio.file.attribute.PosixFilePermission
import scala.build.blooprifle.BloopRifleConfig
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Properties, Random, Success, Try}

object SetupBloopRifle {
  def apply(
      bleepConfig: model.BleepConfig,
      logger: Logger,
      userPaths: UserPaths,
      resolver: Lazy[CoursierResolver],
      bleepRifleLogger: BleepRifleLogger,
      executionContext: ExecutionContext
  ): BloopRifleConfig = {
    val jvm = BleepConfigOps.jvmOrSetDefault(logger, userPaths, bleepConfig)
    val resolvedJvm = FetchJvm(new BleepCacheLogger(logger), jvm, executionContext)

    BloopRifleConfig
      .default(
        BloopRifleConfig.Address.DomainSocket(bspSocketFile(userPaths, bleepConfig.compileServerMode)),
        bloopClassPath(resolver),
        FileUtils.TempDir.toFile
      )
      .copy(
        javaPath = resolvedJvm.toString,
        bspStdout = bleepRifleLogger.bloopBspStdout,
        bspStderr = bleepRifleLogger.bloopBspStderr
      )
  }

  def bloopClassPath(resolver: Lazy[CoursierResolver])(bloopVersion: String): Either[BleepException, (Seq[File], Boolean)] = {
    val modString = BloopRifleConfig.defaultModule
    ModuleParser
      .module(modString, BloopRifleConfig.defaultScalaVersion)
      .left
      .map(msg => new BleepException.ModuleFormatError(modString, msg))
      .flatMap { mod =>
        val dep = model.Dep.Java(mod.organization.value, mod.name.value, bloopVersion)
        val res = resolver.forceGet.force(Set(dep), model.VersionCombo.Java, "installing bloop")
        Right((res.jarFiles, true))
      }
  }

  private def socketDirectory(userPaths: UserPaths, socketId: String): Path = {
    val dir = userPaths.bspSocketDir
    // Ensuring that whenever dir exists, it has the right permissions
    if (!Files.isDirectory(dir)) {
      val tmpDir = dir.getParent / s".${dir.getFileName}.tmp-$socketId"
      try {
        Files.createDirectories(tmpDir)
        if (!Properties.isWin)
          Files.setPosixFilePermissions(
            tmpDir,
            java.util.Set.of(PosixFilePermission.OWNER_EXECUTE, PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE)
          )
        try Files.move(tmpDir, dir, StandardCopyOption.ATOMIC_MOVE)
        catch {
          case _: AtomicMoveNotSupportedException =>
            try Files.move(tmpDir, dir)
            catch {
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

  def bspSocketFile(userPaths: UserPaths, mode: model.CompileServerMode): Path = {
    val somewhatRandomIdentifier = Try(ProcessHandle.current.pid) match {
      case Failure(_) =>
        val r = new Random
        s"conn-${r.nextInt()}"
      case Success(pid) => s"proc-$pid"
    }

    val socketName: String =
      mode match {
        case model.CompileServerMode.NewEachInvocation => somewhatRandomIdentifier
        case model.CompileServerMode.Shared            => "shared"
      }

    val socket: Path =
      socketDirectory(userPaths, somewhatRandomIdentifier) / socketName

    mode match {
      case model.CompileServerMode.NewEachInvocation =>
        Runtime.getRuntime.addShutdownHook(
          new Thread("delete-bloop-bsp-named-socket") {
            override def run() = {
              FileUtils.deleteDirectory(socket)
              ()
            }
          }
        )

      case model.CompileServerMode.Shared => ()
    }

    socket
  }
}
