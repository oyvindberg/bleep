package bleep

import bleep.model.Os

import java.util.Locale

sealed trait OsArch {
  val os: Os
  val arch: Arch
}

object OsArch {
  sealed abstract class HasNativeImage(val os: Os, val arch: Arch) extends OsArch

  object LinuxAmd64 extends HasNativeImage(Os.Linux, Arch.Amd64)
  object WindowsAmd64 extends HasNativeImage(Os.Windows, Arch.Amd64)
  object MacosAmd64 extends HasNativeImage(Os.Macos, Arch.Amd64)
  case class MacosArm64(freedFromJail: Boolean) extends HasNativeImage(Os.Macos, Arch.Arm64)

  case class Other(os: Os, arch: Arch) extends OsArch

  def shouldBeArm64(): Boolean =
    if (sys.env.contains("BLEEP_ALLOW_AMD64")) false
    else {
      // todo: can we discover this without running a process?
      import scala.sys.process.*
      val uname = Seq("uname", "-a").!!.toLowerCase
      uname.contains("arm64")
    }

  lazy val current: OsArch = {
    val os: Os =
      Option(System.getProperty("os.name")).getOrElse("").toLowerCase(Locale.ROOT) match {
        case s if s.contains("windows") => Os.Windows
        case s if s.contains("linux")   => Os.Linux
        case s if s.contains("mac")     => Os.Macos
        case unrecognized               => throw new BleepException.Text(s"OS $unrecognized not supported yet. PR welcome! :)")
      }

    val arch =
      Option(System.getProperty("os.arch")).getOrElse("").toLowerCase(Locale.ROOT) match {
        case "x86_64" | "amd64" => Arch.Amd64
        case "aarch64"          => Arch.Arm64
        case unrecognized       => throw new BleepException.Text(s"Arch $unrecognized not supported yet. PR welcome! :)")
      }

    (os, arch) match {
      case (Os.Windows, Arch.Amd64) => WindowsAmd64
      case (Os.Linux, Arch.Amd64)   => LinuxAmd64
      case (Os.Macos, Arch.Arm64)   => MacosArm64(freedFromJail = false)
      case (Os.Macos, Arch.Amd64)   => if (shouldBeArm64()) MacosArm64(freedFromJail = true) else MacosAmd64
      case (os, arch)               => Other(os, arch)
    }
  }
}
