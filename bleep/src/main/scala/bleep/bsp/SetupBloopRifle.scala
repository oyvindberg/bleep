package bleep
package bsp

import bleep.internal.generateBloopFiles.dependencyOrdering
import bleep.internal.{FileUtils, Lazy}
import coursier.core.Dependency
import coursier.parse.ModuleParser

import java.io.File
import java.nio.file.attribute.PosixFilePermission
import java.nio.file._
import scala.build.blooprifle.BloopRifleConfig
import scala.util.{Failure, Properties, Random, Success, Try}

object SetupBloopRifle {
  def apply(
      javaPath: String,
      userPaths: UserPaths,
      buildPaths: BuildPaths,
      resolver: Lazy[CoursierResolver], // Protocol to use to open a BSP connection with Bloop: tcp | local | default
      bloopBspProtocol: Option[String]
  ): BloopRifleConfig = {
    val tcpOrDomain = bloopBspProtocol match {
      case Some("tcp") => BloopRifleConfig.Address.Tcp(BloopRifleConfig.defaultHost, BloopRifleConfig.defaultPort)
      case _           => BloopRifleConfig.Address.DomainSocket(bspSocketFile(userPaths).toPath)
    }

    BloopRifleConfig.default(tcpOrDomain, bloopClassPath(resolver), buildPaths.dotBleepDir.toFile).copy(javaPath = javaPath)
  }

  def bloopClassPath(resolver: Lazy[CoursierResolver])(bloopVersion: String): Either[BuildException, Seq[File]] = {
    val modString = BloopRifleConfig.defaultModule
    ModuleParser
      .module(modString, BloopRifleConfig.defaultScalaVersion)
      .left
      .map(msg => new BuildException.ModuleFormatError(modString, msg))
      .flatMap { mod =>
        resolver.forceGet(JsonSet(Dependency(mod, bloopVersion)), JsonSet(constants.MavenCentral)) match {
          case Left(coursierError) => Left(new BuildException.ResolveError(coursierError, "installing bloop"))
          case Right(res)          => Right(res.files)
        }
      }
  }
  private lazy val pidOrRandom: Either[Int, Long] =
    Try(ProcessHandle.current.pid) match {
      case Failure(_) =>
        val r = new Random
        Left(r.nextInt())

      case Success(pid) => Right(pid)
    }

  private def bspSocketFile(userPaths: UserPaths): File = {
    val (socket: Path, deleteOnExit: Boolean) = {
      val dir = socketDirectory(userPaths) / pidOrRandom
        .map("proc-" + _)
        .left
        .map("conn-" + _)
        .merge
      (dir, true)
    }
    if (deleteOnExit)
      Runtime.getRuntime.addShutdownHook(
        new Thread("delete-bloop-bsp-named-socket") {
          override def run() = {
            FileUtils.deleteDirectory(socket)
            ()
          }
        }
      )
    socket.toFile.getCanonicalFile
  }

  private def socketDirectory(userPaths: UserPaths): Path = {
    val dir = userPaths.bspSocketDir
    // Ensuring that whenever dir exists, it has the right permissions
    if (!Files.isDirectory(dir)) {
      val tmpDir = dir.getParent / s".${dir.getFileName}.tmp-${pidOrRandom.merge}"
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
}
