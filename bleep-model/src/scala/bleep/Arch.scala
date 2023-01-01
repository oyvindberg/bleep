package bleep

sealed trait Arch

object Arch {
  case object Amd64 extends Arch
  case object Arm64 extends Arch
}
