package bleep

import java.util.Locale

sealed trait OsArch {
  val os: model.Os
  val arch: Arch
}

object OsArch {
  sealed abstract class HasNativeImage(val os: model.Os, val arch: Arch) extends OsArch

  object LinuxAmd64 extends HasNativeImage(model.Os.Linux, Arch.Amd64)
  object WindowsAmd64 extends HasNativeImage(model.Os.Windows, Arch.Amd64)
  object MacosAmd64 extends HasNativeImage(model.Os.Macos, Arch.Amd64)
  case class MacosArm64(freedFromJail: Boolean) extends HasNativeImage(model.Os.Macos, Arch.Arm64)

  case class Other(os: model.Os, arch: Arch) extends OsArch

  def shouldBeArm64(): Boolean =
    if (sys.env.contains("BLEEP_ALLOW_AMD64")) false
    else {
      // todo: can we discover this without running a process?
      import scala.sys.process.*
      val uname = Seq("uname", "-a").!!.toLowerCase
      uname.contains("arm64")
    }

  lazy val current: OsArch = {
    val os: model.Os =
      Option(System.getProperty("os.name")).getOrElse("").toLowerCase(Locale.ROOT) match {
        case s if s.contains("windows") => model.Os.Windows
        case s if s.contains("linux")   => model.Os.Linux
        case s if s.contains("mac")     => model.Os.Macos
        case unrecognized               => throw new BleepException.Text(s"OS $unrecognized not supported yet. PR welcome! :)")
      }

    val arch =
      Option(System.getProperty("os.arch")).getOrElse("").toLowerCase(Locale.ROOT) match {
        case "x86_64" | "amd64" => Arch.Amd64
        case "aarch64"          => Arch.Arm64
        case unrecognized       => throw new BleepException.Text(s"Arch $unrecognized not supported yet. PR welcome! :)")
      }

    (os, arch) match {
      case (model.Os.Windows, Arch.Amd64) => WindowsAmd64
      case (model.Os.Linux, Arch.Amd64)   => LinuxAmd64
      case (model.Os.Macos, Arch.Arm64)   => MacosArm64(freedFromJail = false)
      case (model.Os.Macos, Arch.Amd64)   => if (shouldBeArm64()) MacosArm64(freedFromJail = true) else MacosAmd64
      case (os, arch)                     => Other(os, arch)
    }
  }
}
