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
      val latestRelease = model.BleepVersion.current.latestRelease
      OsArch.current match {
        case image: OsArch.HasNativeImage if !forceJvm =>
          pre.logger.warn(s"couldn't determine name of current Bleep executable. Setting up version ${latestRelease.value}")
          val bin = FetchBleepRelease(latestRelease, pre.cacheLogger, pre.ec, image).orThrow
          DownloadedBinary(bin)
        case other =>
          if (forceJvm) pre.logger.info(s"Setting up Bleep through a JVM as requested")
          else pre.logger.warn(s"There is no published graalvm native-image for $other. Setting up Bleep through a JVM")

          val bleepCliDep = model.Dep.ScalaDependency(Organization("build.bleep"), ModuleName("bleep-cli"), latestRelease.value, fullCrossVersion = false)
          val resolvedBleep = resolver.force(
            Set(bleepCliDep),
            model.VersionCombo.Jvm(model.VersionScala.Scala213),
            libraryVersionSchemes = SortedSet.empty,
            context = s"resolving bleep ${latestRelease.value} from maven central"
          )

          DownloadedJava(
            pre.resolvedJvm.forceGet.javaBin,
            jvmRunCommand.cmdArgs(jvmOptions = Nil, cp = resolvedBleep.jars, main = BleepMain, args = Nil)
          )
      }
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
