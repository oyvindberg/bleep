package bleep

import bleep.internal.generateBloopFiles.ordering
import bleep.internal.Os
import coursier.core.Dependency
import coursier.parse.ModuleParser

import java.io.File
import java.nio.file._
import java.nio.file.attribute.PosixFilePermission
import java.util
import java.util.{Locale, Random}
import scala.build.blooprifle.{BloopRifleConfig, BspConnectionAddress}
import scala.util.{Failure, Properties, Success, Try}

class BloopSetup(
    javaPath: String,
    started: Started,
    // Protocol to use to open a BSP connection with Bloop: tcp | local | default
    bloopBspProtocol: Option[String],
    // Socket file to use to open a BSP connection with Bloop (on Windows, a pipe name like "`\\.\pipe\…`")
    bloopBspSocket: Option[String]
) {
  lazy val bloopRifleConfig = {
    val baseConfig = BloopRifleConfig.default(bloopClassPath)

    baseConfig.copy(
      javaPath = javaPath,
      bspSocketOrPort = None // defaultBspSocketOrPort
    )
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
      if (Properties.isWin)
        Some(() => BspConnectionAddress.WindowsNamedPipe(bspPipeName))
      else
        Some(() => BspConnectionAddress.UnixDomainSocket(bspSocketFile()))

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

  private def bspPipeName: String =
    bloopBspSocket.filter(_.nonEmpty).getOrElse {
      val bt = "\\"
      s"$bt$bt.${bt}pipe$bt" + pidOrRandom
        .map("proc-" + _)
        .left
        .map("conn-" + _)
        .merge
    }

  private lazy val pidOrRandom: Either[Int, Long] =
    Try(ProcessHandle.current.pid) match {
      case Failure(_) =>
        val r = new Random
        Left(r.nextInt())

      case Success(pid) => Right(pid)
    }

  private def bspSocketFile(): File = {
    val (socket, deleteOnExit) = bloopBspSocket match {
      case Some(path) =>
        (Os.cwd / path, false)
      case None =>
        val dir = socketDirectory()
        val fileName = pidOrRandom
          .map("proc-" + _)
          .left
          .map("conn-" + _)
          .merge
        val path = dir / fileName
        if (Files.exists(path)) // isFile is false for domain sockets
          Files.delete(path)
        (path, true)
    }
    if (deleteOnExit)
      Runtime.getRuntime.addShutdownHook(
        new Thread("delete-bloop-bsp-named-socket") {
          override def run() = {
            Files.deleteIfExists(socket)
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
          Files.setPosixFilePermissions(tmpDir, util.Set.of(PosixFilePermission.OWNER_EXECUTE, PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE))
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
