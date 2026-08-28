package bleep

import bleep.internal.compat.*
import bleep.internal.jvmRunCommand
import coursier.core.{ModuleName, Organization}
import ryddig.Logger

import java.lang.management.ManagementFactory
import java.nio.file.{Files, Path}
import scala.collection.immutable.SortedSet

sealed trait BleepExecutable {
  def command: Path
  def args: List[String]
  def whole: List[String] = command.toString +: args
}

object BleepExecutable {
  private val BleepMain = "bleep.Main"

  sealed trait Binary extends BleepExecutable {
    def command: Path
    override def args: List[String] = Nil
  }

  case class CurrentBinary(command: Path) extends Binary
  case class DownloadedBinary(command: Path) extends Binary
  case class InheritedBinary(command: Path) extends Binary
  case class CurrentJava(command: Path, args: List[String]) extends BleepExecutable
  case class DownloadedJava(command: Path, args: List[String]) extends BleepExecutable

  def getCommand(resolver: CoursierResolver, pre: Prebootstrapped, forceJvm: Boolean): BleepExecutable =
    findCurrentBleep(pre.logger).getOrElse {
      // The version we are, not the last release we descend from.
      //
      // This used to ask for `BleepVersion.current.latestRelease`, which drops the `+<n>-<sha>-SNAPSHOT` suffix — so a snapshot bleep handed out a *stable*
      // bleep as its own relaunch command. That is not a smaller version of the right answer, it is the wrong one: what `getCommand` returns is written into
      // launch configuration that outlives the session (`SetupIde` puts it in the BSP connection file, `SetupMcpServer` in the MCP command), so an IDE would go
      // on starting a bleep that disagrees with both the build's `$version` and the jars published beside it. Client, server and test-runner must be one
      // version; this is one of the places that decides.
      //
      // Asking for `current` also reaches the snapshot handling that already exists for it: [[FetchBleepRelease]] routes development versions to
      // [[FetchBleepSnapshot]] — its own comment names this caller as a beneficiary, which it could not be while the suffix was being stripped here first.
      // Where that cannot deliver, [[fetchBinaryOrLastPublished]] says so and takes the release instead, rather than leaving the caller with nothing.
      val wanted = model.BleepVersion.current
      OsArch.current match {
        case image: OsArch.HasNativeImage if !forceJvm =>
          pre.logger.warn(s"couldn't determine name of current Bleep executable. Setting up version ${wanted.value}")
          DownloadedBinary(fetchBinaryOrLastPublished(wanted, pre, image))
        case other =>
          if (forceJvm) pre.logger.info(s"Setting up Bleep through a JVM as requested")
          else pre.logger.warn(s"There is no published graalvm native-image for $other. Setting up Bleep through a JVM")

          // A locally published snapshot resolves from `~/.ivy2/local`, which `DefaultRepos` searches ahead of Maven Central, so this path needs no network
          // when the jars were published beside the binary.
          val bleepCliDep = model.Dep.ScalaDependency(Organization("build.bleep"), ModuleName("bleep-cli"), wanted.value, fullCrossVersion = false)
          val resolvedBleep = resolver.force(
            Set(bleepCliDep),
            model.VersionCombo.Jvm(model.VersionScala.Scala213),
            libraryVersionSchemes = SortedSet.empty,
            context = s"resolving bleep ${wanted.value}",
            model.IgnoreEvictionErrors.No
          )

          DownloadedJava(
            pre.resolvedJvm.forceGet.javaBin,
            jvmRunCommand.cmdArgs(jvmOptions = Nil, cp = resolvedBleep.jars, main = BleepMain, args = Nil)
          )
      }
    }

  /** The bleep we are, if it can be had — otherwise the release it descends from, said out loud.
    *
    * For a release there is nothing to fall back from: the download URL is built from the version. A *snapshot* is different in kind. It lives in the artifacts
    * of its CI run, which need a token and expire after a week, so on a machine that has neither there is no way to produce this exact bleep, and no amount of
    * correctness in the version we ask for changes that.
    *
    * Degrading rather than failing is a judgement about who is asking. `bleepscript.Started.bleepExecutable` hands this to a user's script so it can invoke
    * bleep again, and a script that cannot run bleep at all is worse off than one running the previous release. So this is deliberately not the usual
    * fail-loudly: it is fail-loudly-then-continue, and the warning names both versions because they are not interchangeable — anything version-sensitive the
    * script goes on to do will happen under the release, not under this snapshot.
    */
  private def fetchBinaryOrLastPublished(wanted: model.BleepVersion, pre: Prebootstrapped, image: OsArch.HasNativeImage): Path =
    FetchBleepRelease(wanted, pre.cacheLogger, pre.ec, image) match {
      case Right(bin) => bin
      // Only a snapshot has somewhere to fall back to. For a release `latestRelease` is the same version, so retrying would just fail the same way twice.
      case Left(err) if wanted.isDevelopment =>
        val published = wanted.latestRelease
        pre.logger.warn(s"couldn't fetch bleep ${wanted.value}: ${err.getMessage}")
        pre.logger.warn(
          s"falling back to ${published.value}, the last release ${wanted.value} descends from. Whatever runs this bleep will run that version, not the one you are on."
        )
        FetchBleepRelease(published, pre.cacheLogger, pre.ec, image).orThrow
      case Left(err) => throw err
    }

  def findCurrentBleep(logger: Logger): Option[BleepExecutable] = {
    def complain(msg: String, currentInfo: Option[ProcessHandle.Info] = None): None.type = {
      logger
        .withOptContext("processHandle.info", currentInfo.map(_.toString))
        .warn(s"Problem while discovering how to run the bleep you started: $msg. Falling back to downloading latest release.")
      None
    }

    val ret = {
      val currentInfo: ProcessHandle.Info = ProcessHandle.current.info()

      currentInfo.command().toScalaCompat.flatMap { commandString =>
        Path.of(commandString).toAbsolutePath match {
          case cmd if !Files.exists(cmd) =>
            complain(s"Obtained command $commandString which does not exist", Some(currentInfo))
          case command if command.getFileName.toString.contains("java") =>
            // note: we cannot use `currentInfo.args()` since it's not available in some contexts like docker containers
            val mxBean = ManagementFactory.getRuntimeMXBean

            val cp = mxBean.getClassPath

            if (cp.split(java.io.File.pathSeparator).exists(_.contains("bleep-cli"))) {
              val filteredArgs = mxBean.getInputArguments.toScalaCompat
                // remove intellij-specific flags for debugging, which may cause subprocesses to port clash with main process
                .filterNot(_.contains("-agentlib:"))
                .filterNot(_.contains("-javaagent:"))
                // don't keep pwd set from original java command
                .filterNot(_.contains("-Duser.dir"))

              // `mxBean.getInputArguments` does not include class name and classpath, so reconstruct that here
              val args = filteredArgs ++ List("-cp", cp, BleepMain)

              Some(CurrentJava(command, args))
            } else complain(s"Obtained JVM class path which didn't include bleep-cli")

          case command =>
            Some(CurrentBinary(command))
        }
      }
    }

    ret.foreach { cmd =>
      logger.withContext("cmd", cmd.whole).debug("Can run your current bleep with this command")
    }

    ret
  }
}
