package bleep
package bsp

import bleep.internal.FileUtils
import bleep.model.CompileServerMode
import bloop.rifle.BloopRifleConfig

import java.io.File
import java.nio.file.*
import java.nio.file.attribute.PosixFilePermission
import scala.collection.immutable.SortedSet
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Properties, Random, Success, Try}

object SetupBloopRifle {
  def apply(
      compileServerMode: CompileServerMode,
      resolvedJvm: ResolvedJvm,
      userPaths: UserPaths,
      resolver: CoursierResolver,
      bleepRifleLogger: BleepRifleLogger
  ): BloopRifleConfig = {
    val default = BloopRifleConfig
      .default(
        BloopRifleConfig.Address.DomainSocket(bspSocketFile(userPaths, compileServerMode, resolvedJvm.jvm)),
        bloopClassPath(resolver),
        FileUtils.TempDir.toFile
      )

    default.copy(
      javaPath = resolvedJvm.javaBin.toString,
      bspStdout = bleepRifleLogger.bloopBspStdout,
      bspStderr = bleepRifleLogger.bloopBspStderr,
      period = 10.millis,
      javaOpts = default.javaOpts
    )
  }

  val parallelCollectionAlways = model.LibraryVersionScheme(
    model.LibraryVersionScheme.VersionScheme.Always,
    model.Dep.Scala("org.scala-lang.modules", "scala-parallel-collections", "always")
  )
  val versionCombo = model.VersionCombo.Jvm(model.VersionScala.Scala213)

  def bloopClassPath(resolver: CoursierResolver)(bloopVersion: String): Either[BleepException, (Seq[File], Boolean)] = {
    val dep = model.Dep.Scala("io.github.alexarchambault.bleep", "bloop-frontend", bloopVersion)
    resolver
      .updatedParams(_.copy(downloadSources = false))
      .resolve(Set(dep), versionCombo, libraryVersionSchemes = SortedSet(parallelCollectionAlways)) match {
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
          jvm.name.replace(':', '_')
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
