package bleep
package bsp

import bleep.bsp.CompileServerMode.{NewEachInvocation, Shared}
import bleep.internal.{dependencyOrdering, FileUtils, Lazy}
import coursier.core.Dependency
import coursier.parse.ModuleParser

import java.io.File
import java.nio.file.{AtomicMoveNotSupportedException, FileAlreadyExistsException, Files, Path, StandardCopyOption}
import java.nio.file.attribute.PosixFilePermission
import scala.build.blooprifle.BloopRifleConfig
import scala.util.{Failure, Properties, Random, Success, Try}

object SetupBloopRifle {
  def apply(jvm: Path, pre: Prebootstrapped, resolver: Lazy[CoursierResolver], mode: CompileServerMode): BloopRifleConfig =
    BloopRifleConfig
      .default(
        BloopRifleConfig.Address.DomainSocket(bspSocketFile(pre.userPaths, mode)),
        bloopClassPath(resolver),
        pre.buildPaths.dotBleepDir.toFile
      )
      .copy(javaPath = jvm.toString)

  def bloopClassPath(resolver: Lazy[CoursierResolver])(bloopVersion: String): Either[BuildException, (Seq[File], Boolean)] = {
    val modString = BloopRifleConfig.defaultModule
    ModuleParser
      .module(modString, BloopRifleConfig.defaultScalaVersion)
      .left
      .map(msg => new BuildException.ModuleFormatError(modString, msg))
      .flatMap { mod =>
        resolver.forceGet(JsonSet(Dependency(mod, bloopVersion)), forceScalaVersion = None) match {
          case Left(coursierError) => Left(new BuildException.ResolveError(coursierError, "installing bloop"))
          case Right(res)          => Right((res.jarFiles, false))
        }
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

  def bspSocketFile(userPaths: UserPaths, mode: CompileServerMode): Path = {
    val somewhatRandomIdentifier = Try(ProcessHandle.current.pid) match {
      case Failure(_) =>
        val r = new Random
        s"conn-${r.nextInt()}"
      case Success(pid) => s"proc-$pid"
    }

    val socketName: String =
      mode match {
        case NewEachInvocation => somewhatRandomIdentifier
        case Shared            => "shared"
      }

    val socket: Path =
      socketDirectory(userPaths, somewhatRandomIdentifier) / socketName

    mode match {
      case NewEachInvocation =>
        Runtime.getRuntime.addShutdownHook(
          new Thread("delete-bloop-bsp-named-socket") {
            override def run() = {
              FileUtils.deleteDirectory(socket)
              ()
            }
          }
        )

      case Shared =>
    }

    socket
  }
}
