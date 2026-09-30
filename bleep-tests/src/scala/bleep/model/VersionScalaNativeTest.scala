package bleep
package model

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Path

class VersionScalaNativeTest extends AnyFunSuite {
  private val dirs = List(Path.of("/build/core/native/src/scala"), Path.of("/build/core/shared/src/scala"), Path.of("/build/core/native/src/scala"))

  test("the plugin is told the source directories, sorted and each once, like sbt-scala-native does") {
    val opt = VersionScalaNative("0.5.12").positionRelativizationPaths(dirs)
    // as this platform writes the paths: `\build\core\...` on windows
    val expected = List(dirs(0), dirs(1)).map(_.toString).mkString(";")
    assert(opt === Some(Options.Opt.Flag(s"-P:scalanative:positionRelativizationPaths:$expected")))
    assert(opt.forall(VersionScalaNative.isPositionRelativizationPaths))
  }

  test("a plugin older than 0.5 does not know the option") {
    assert(VersionScalaNative("0.4.17").positionRelativizationPaths(dirs) === None)
  }
}
