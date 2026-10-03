package bleep.bsp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Which collector the compile server runs with, by the JDK it runs on.
  *
  * The server runs on the build's JDK. ZGC was set for every JDK, and before 21 it is not generational: a build on JDK 17 ran its compiles under a collector
  * that cycled back to back, and a clean build of scala3's compiler took 55–69 s against 39–51 s under G1.
  */
class ServerGcOptionsTest extends AnyFunSuite with Matchers {

  private def gcOptions(jdk: Int): Seq[String] =
    (BspRifleConfig.defaultJavaOpts ++ BspRifleConfig.jdkVersionOpts(jdk)).filter(o => o.contains("ZGC") || o.contains("ZGenerational") || o.contains("G1GC"))

  test("before JDK 21, no collector is chosen, so the JVM runs its default (G1)") {
    gcOptions(17) shouldBe empty
  }

  test("a JDK whose version cannot be read gets the JVM's default too") {
    gcOptions(0) shouldBe empty
  }

  test("from JDK 21, generational ZGC: asked for through 23, the only kind from 24") {
    gcOptions(21) should contain allOf ("-XX:+UseZGC", "-XX:+ZGenerational")
    gcOptions(23) should contain allOf ("-XX:+UseZGC", "-XX:+ZGenerational")
    gcOptions(25) should contain("-XX:+UseZGC")
    gcOptions(25) should not contain "-XX:+ZGenerational"
  }
}
