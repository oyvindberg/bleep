package bleep.sbtimport

import bleep.{model, RelPath}
import org.scalatest.funsuite.AnyFunSuite

class TemplatizeSourcePathTest extends AnyFunSuite {
  private def templatize(platform: model.PlatformId, path: String): String = templatizeFor("2.13.16", platform, path)

  private def templatizeFor(scalaVersion: String, platform: model.PlatformId, path: String): String = {
    val replacements = model.Replacements.versions(
      None,
      scalaVersion = Some(model.VersionScala(scalaVersion)),
      platform = Some(platform),
      platformVersion = None,
      includeEpoch = false,
      includeBinVersion = true,
      buildDir = None
    )
    buildFromBloopFiles.templatizeSourcePath(replacements)(RelPath.force(path)).asString
  }

  test("a directory for one platform is written for every platform") {
    assert(templatize(model.PlatformId.Jvm, "src/main/scala-jvm") === "src/main/scala-${PLATFORM}")
    assert(templatize(model.PlatformId.Jvm, "src/main/scala-2.13") === "src/main/scala-${SCALA_BIN_VERSION}")
  }

  // tapir's `superMatrixSettings`, and the directories sbt-crossproject shares between two of three platforms
  test("a directory several platforms share keeps its name") {
    assert(templatize(model.PlatformId.Jvm, "src/main/scala-js-jvm") === "src/main/scala-js-jvm")
    assert(templatize(model.PlatformId.Js, "src/main/scala-js-jvm") === "src/main/scala-js-jvm")
    assert(templatize(model.PlatformId.Native, "jvm-native/src/test/scala-2.13") === "jvm-native/src/test/scala-${SCALA_BIN_VERSION}")
  }

  // tapir's `versionedScalaSourceDirectories`: code for scala 3 and 2.13 and later
  test("a directory several scala versions share keeps its name") {
    // on scala 3 the `3` is the binary version, which would make it `scala-${SCALA_BIN_VERSION}-2.13+` there and `scala-3-2.13+` on 2.13
    assert(templatizeFor("3.3.6", model.PlatformId.Jvm, "src/main/scala-3-2.13+") === "src/main/scala-3-2.13+")
    assert(templatizeFor("3.3.6", model.PlatformId.Jvm, "src/main/scalajvm-3-2.13+") === "src/main/scalajvm-3-2.13+")
    assert(templatizeFor("3.3.6", model.PlatformId.Jvm, "src/main/scala-3") === "src/main/scala-${SCALA_BIN_VERSION}")
  }
}
