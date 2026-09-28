package bleep
package model

import org.scalatest.funsuite.AnyFunSuite

/** `cross-full` mirrors sbt-crossproject's `CrossType.Full`, including the directories it shares between some but not all platforms */
class SourceLayoutTest extends AnyFunSuite {
  import PlatformId.{Js, Jvm, Native}

  private val scala213 = Some(VersionScala("2.13.16"))
  private val scala3 = Some(VersionScala("3.3.6"))

  private def sources(scalaVersion: Option[VersionScala], platformId: PlatformId, crossPlatforms: Set[PlatformId]): List[String] =
    SourceLayout.CrossFull.sources(scalaVersion, Some(platformId), crossPlatforms, "main").values.toList.map(_.asString)

  test("a project built for three platforms shares a directory with each other platform") {
    assert(SourceLayout.CrossFull.partiallySharedDirs(Js, Set(Jvm, Js, Native)) === List("js-jvm", "js-native"))
    assert(SourceLayout.CrossFull.partiallySharedDirs(Jvm, Set(Jvm, Js, Native)) === List("js-jvm", "jvm-native"))
    assert(SourceLayout.CrossFull.partiallySharedDirs(Native, Set(Jvm, Js, Native)) === List("js-native", "jvm-native"))
  }

  test("a project built for two platforms shares all its code in shared/") {
    assert(SourceLayout.CrossFull.partiallySharedDirs(Jvm, Set(Jvm, Js)) === Nil)
    assert(SourceLayout.CrossFull.partiallySharedDirs(Jvm, Set(Jvm)) === Nil)
  }

  test("a platform the project is not built for is a bug in the caller") {
    assertThrows[BleepException](SourceLayout.CrossFull.partiallySharedDirs(Js, Set(Jvm, Native)))
  }

  test("the shared directories have the same scala version directories as shared/, like sbt-crossproject's") {
    val jsSources = sources(scala213, Js, Set(Jvm, Js, Native))
    List("js-jvm", "js-native", "shared", "js").foreach { dir =>
      List("scala", "scala-2.13", "scala-2").foreach(sub => assert(jsSources.contains(s"$dir/src/main/$sub"), s"$dir/src/main/$sub"))
    }
    assert(!jsSources.exists(_.startsWith("jvm-native/")))

    val jvm3Sources = sources(scala3, Jvm, Set(Jvm, Js, Native))
    assert(jvm3Sources.contains("jvm-native/src/main/scala-3"))
    assert(jvm3Sources.contains("js-jvm/src/main/scala"))
  }

  test("sbt-matrix has the directories sbt-projectmatrix adds for a platform") {
    val sources = SourceLayout.SbtMatrix.sources(scala213, Some(Js), Set(Jvm, Js), "main").values.toList.map(_.asString)
    List("src/main/scalajs", "src/main/scalajs-2.13", "src/main/javajs").foreach(dir => assert(sources.contains(dir), dir))
  }

  test("resources too") {
    val resources = SourceLayout.CrossFull.resources(scala213, Some(Native), Set(Jvm, Js, Native), "test").values.toList.map(_.asString)
    assert(resources.sorted === List("js-native/src/test/resources", "jvm-native/src/test/resources", "native/src/test/resources", "shared/src/test/resources"))
  }
}
