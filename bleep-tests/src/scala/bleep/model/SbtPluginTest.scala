package bleep
package model

import coursier.core.{ModuleName, Organization}
import io.circe.syntax.*
import org.scalatest.funsuite.AnyFunSuite

class SbtPluginTest extends AnyFunSuite {
  private val dynver = Dep.ScalaDependency(Organization("com.github.sbt"), ModuleName("sbt-dynver"), "5.1.1", fullCrossVersion = false, isSbtPlugin = true)

  test("an org::name sbt plugin is an sbt 1 plugin for a scala 2.12 project: the base name, with sbt 1's attributes") {
    val java = dynver.asJava(VersionCombo.Jvm(VersionScala.Scala212)).orThrowText
    assert(java.moduleName == ModuleName("sbt-dynver"))
    assert(java.isSbtPlugin)
    assert(java.dependency.module.attributes == Dep.SbtPluginAttrs)
  }

  test("an org::name sbt plugin is an sbt 2 plugin for a scala 3 project: its own name, no attributes") {
    val java = dynver.asJava(VersionCombo.Jvm(VersionScala.Scala3)).orThrowText
    assert(java.moduleName == ModuleName("sbt-dynver_sbt2_3"))
    assert(!java.isSbtPlugin)
    assert(java.dependency.module.attributes.isEmpty)
  }

  test("sbt plugins are scala 2.12 or 3, and run on the jvm") {
    assert(dynver.asJava(VersionCombo.Jvm(VersionScala.Scala213)).isLeft)
    assert(dynver.asJava(VersionCombo.Js(VersionScala.Scala3, VersionScalaJs.ScalaJs1)).isLeft)
  }

  test("isSbtPlugin on an org::name dependency survives the build file") {
    val json = (dynver: Dep).asJson
    assert(json.hcursor.get[Boolean]("isSbtPlugin") == Right(true))
    assert(json.as[Dep] == Right(dynver))
  }

  test("the sbt a plugin project is for, and the name it is published under") {
    assert(Scala.SbtPlugin.forScalaVersion(VersionScala.Scala212).map(_.artifactName("sbt-dynver")) == Right("sbt-dynver_2.12_1.0"))
    assert(Scala.SbtPlugin.forScalaVersion(VersionScala.Scala3).map(_.artifactName("sbt-dynver")) == Right("sbt-dynver_sbt2_3"))
    assert(Scala.SbtPlugin.forScalaVersion(VersionScala.Scala213).isLeft)
  }

  test("scala.sbtPlugin survives the build file, and templates") {
    val scala = Scala(Some(VersionScala.Scala212), Options.empty, None, JsonSet.empty, None, None, None, Some(true))
    assert(scala.asJson.as[Scala] == Right(scala))
    assert(scala.intersect(scala).sbtPlugin == Some(true))
    assert(scala.removeAll(scala).sbtPlugin == None)
  }
}
