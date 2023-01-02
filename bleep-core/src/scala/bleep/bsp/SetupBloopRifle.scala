package bleep
package bsp

import bleep.internal.FileUtils
import bleep.logging.Logger
import bleep.model.CompileServerMode
import coursier.parse.ModuleParser

import java.io.File
import java.nio.file._
import java.nio.file.attribute.PosixFilePermission
import scala.build.blooprifle.BloopRifleConfig
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Properties, Random, Success, Try}

object SetupBloopRifle {
  def apply(
      compileServerMode: CompileServerMode,
      jvm: model.Jvm,
      logger: Logger,
      userPaths: UserPaths,
      resolver: CoursierResolver,
      bleepRifleLogger: BleepRifleLogger,
      executionContext: ExecutionContext
  ): BloopRifleConfig = {
    val resolvedJvm = FetchJvm(Some(userPaths.resolveJvmCacheDir), new BleepCacheLogger(logger), jvm, executionContext)

    BloopRifleConfig
      .default(
        BloopRifleConfig.Address.DomainSocket(bspSocketFile(userPaths, compileServerMode, jvm)),
        bloopClassPath(resolver),
        FileUtils.TempDir.toFile
      )
      .copy(
        javaPath = resolvedJvm.toString,
        bspStdout = bleepRifleLogger.bloopBspStdout,
        bspStderr = bleepRifleLogger.bloopBspStderr,
        period = 10.millis
      )
  }

  def bloopClassPath(resolver: CoursierResolver)(bloopVersion: String): Either[BleepException, (Seq[File], Boolean)] =
    ModuleParser.module(BloopRifleConfig.defaultModule, BloopRifleConfig.defaultScalaVersion) match {
      case Left(msg) => Left(new BleepException.ModuleFormatError(BloopRifleConfig.defaultModule, msg))
      case Right(mod) =>
        val dep = model.Dep.JavaDependency(mod.organization, mod.name, bloopVersion)
        resolver.resolve(Set(dep), model.VersionCombo.Java) match {
          case Left(coursierError) =>
            Left(new BleepException.ResolveError(coursierError, "installing bloop"))
          case Right(value) =>
            Right((value.jarFiles, true))
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
  val SharedPrefix = "shared-for-jvm"

  def bspSocketFile(userPaths: UserPaths, mode: model.CompileServerMode, jvm: model.Jvm): Path = {
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
          val str = jvm.name.replace(':', '_')
          s"$SharedPrefix-$str"
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
