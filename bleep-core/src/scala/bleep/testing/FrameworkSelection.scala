package bleep.testing

/** How a discovered suite is to be executed.
  *
  * Decided on the server, which has the project's classpath in front of it, and carried to the fork over `TestProtocol`. The fork used to receive only a
  * framework *display name* — "JUnit Jupiter", "Kotest", "ScalaTest" — and reconstruct the decision from it with a substring match and a hand-maintained
  * spelling table. Two lists that had to agree, connected by prose, and they didn't: "Spock", "kotlin.test", "ScalaCheck" and "uTest" all reached
  * `Class.forName` as literal names and killed the fork, because a display name is a label a framework picks for itself, not an identifier.
  *
  * The display name survives, but only as a label — for logs, for BSP's `ScalaTestClassesItem`, for metrics.
  */
sealed trait FrameworkSelection {

  /** What a human should see. Never dispatched on. */
  def displayName: String
}

object FrameworkSelection {

  /** Run through `org.junit.platform.launcher.Launcher`, driven by `JUnitPlatformRunner`. Every engine on the JUnit Platform lands here — jupiter, vintage,
    * kotest, spock, jqwik, cucumber — because the platform is itself a runner SPI and one Launcher call reaches all of them.
    */
  case class JUnitPlatform(displayName: String) extends FrameworkSelection

  /** Run through `sbt.testing.Framework`, instantiated from exactly this class. The class name is the framework's real identifier: the fork's only job is
    * `Class.forName(frameworkClass).getDeclaredConstructor().newInstance()`, with nothing left to guess.
    */
  case class SbtTestInterface(displayName: String, frameworkClass: String) extends FrameworkSelection

  /** Run by the platform's own runner — Scala.js, Scala Native, Kotlin/JS, Kotlin/Native — which links a test binary rather than forking a JVM.
    *
    * These never reach `TestProtocol`: the test handler branches on the project's platform first, and only the JVM case talks to a fork. The case exists so
    * that "no JVM runner applies" is stated rather than encoded as a placeholder class name, and so that sending one to a fork fails loudly instead of
    * producing a command the other side would have to interpret.
    */
  case class PlatformRunner(displayName: String) extends FrameworkSelection
}
