package bleep

import bleep.internal.FileUtils
import bleep.internal.generateBloopFiles.ordering
import coursier.core.Dependency
import coursier.parse.ModuleParser

import java.io.File
import java.nio.file._
import java.nio.file.attribute.PosixFilePermission
import java.util.Locale
import scala.build.blooprifle.{BloopRifleConfig, BspConnectionAddress}
import scala.util.{Failure, Properties, Random, Success, Try}

class BloopSetup(
    javaPath: String,
    started: Started,
    // Protocol to use to open a BSP connection with Bloop: tcp | local | default
    bloopBspProtocol: Option[String]
) {
  lazy val bloopRifleConfig = {
    val tcpOrDomain = bloopBspProtocol match {
      case Some("tcp") => BloopRifleConfig.Address.Tcp(BloopRifleConfig.defaultHost, BloopRifleConfig.defaultPort)
      case _           => BloopRifleConfig.Address.DomainSocket(bspSocketFile.toPath)
    }

    BloopRifleConfig.default(tcpOrDomain, bloopClassPath, started.buildPaths.dotBleepDir.toFile).copy(javaPath = javaPath)
  }

  def bloopClassPath: String => Either[BuildException, Seq[File]] = {
    val modString = BloopRifleConfig.defaultModule
    (bloopVersion: String) =>
      ModuleParser
        .module(modString, BloopRifleConfig.defaultScalaVersion)
        .left
        .map(msg => new BuildException.ModuleFormatError(modString, msg))
        .flatMap { mod =>
          started.resolver(JsonSet(Dependency(mod, bloopVersion)), started.build.resolvers) match {
            case Left(coursierError) => Left(new BuildException.ResolveError(coursierError, "installing bloop"))
            case Right(res)          => Right(res.files)
          }
        }
  }

  private def isGraalvmNativeImage: Boolean =
    sys.props.contains("org.graalvm.nativeimage.imagecode")

  private def arch = sys.props("os.arch").toLowerCase(Locale.ROOT) match {
    case "amd64" => "x86_64"
    case other   => other
  }

  def defaultBspSocketOrPort: Option[() => BspConnectionAddress] = {
    def namedSocket =
      Some(() => BspConnectionAddress.UnixDomainSocket(bspSocketFile))

    // FreeBSD and others throw a java.lang.UnsatisfiedLinkError when trying the
    // UnixDomainSocket, because of the ipcsocket JNI stuff, so stick with TCP for them.
    def isStandardOs = Properties.isLinux || Properties.isWin || Properties.isMac
    def default =
      if ((isGraalvmNativeImage && arch != "x86_64") || !isStandardOs)
        None // tcp
      else
        namedSocket
    bloopBspProtocol.filter(_ != "default") match {
      case None          => default
      case Some("tcp")   => None
      case Some("local") => namedSocket
      case Some(other) =>
        sys.error(
          s"Invalid bloop BSP protocol value: '$other' (expected 'tcp', 'local', or 'default')"
        )
    }
  }

  private lazy val pidOrRandom: Either[Int, Long] =
    Try(ProcessHandle.current.pid) match {
      case Failure(_) =>
        val r = new Random
        Left(r.nextInt())

      case Success(pid) => Right(pid)
    }

  private def bspSocketFile: File = {
    val (socket: Path, deleteOnExit: Boolean) = {
      val dir = socketDirectory() / pidOrRandom
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
            if (Files.exists(socket)) {
              FileUtils.deleteDirectory(socket)
            }
            ()
          }
        }
      )
    socket.toFile.getCanonicalFile
  }

  private def socketDirectory(): Path = {
    val dir = started.directories.bspSocketDir
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
