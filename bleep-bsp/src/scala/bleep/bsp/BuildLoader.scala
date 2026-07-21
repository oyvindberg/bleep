package bleep.bsp

object BuildLoader {

  /** Platform classification for a project */
  sealed trait Platform
  object Platform {
    case object Jvm extends Platform
    case class ScalaJs(sjsVersion: String, scalaVersion: String) extends Platform
    case class ScalaNative(snVersion: String, scalaVersion: String) extends Platform
    case class KotlinJs(kotlinVersion: String) extends Platform
    case class KotlinNative(kotlinVersion: String) extends Platform
  }
}
