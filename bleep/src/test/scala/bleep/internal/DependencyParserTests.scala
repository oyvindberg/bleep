package bleep
package internal

import bleep.internal.DependencyParser.javaOrScalaDependencyParams
import coursier.core._
import coursier.{moduleNameString, moduleString, organizationString, Attributes, Dependency}
import org.scalatest.funsuite.AnyFunSuite

class DependencyParserTests extends AnyFunSuite {

  val url = "file%3A%2F%2Fsome%2Fencoded%2Furl"
  private val scala211 = bleep.Versions.Scala("2.11.11")
  private val scala212 = bleep.Versions.Scala("2.12.8")

  def dependencyParams(
      input: String,
      scalaVersion: bleep.Versions.Scala,
      defaultConfiguration: Configuration = Configuration.empty,
      platformName: String = ""
  ): Either[String, (Dependency, Map[String, String])] =
    javaOrScalaDependencyParams(input, defaultConfiguration).map { case (dep, params) =>
      (dep.dependency(scalaVersion, platformName), params)
    }

  // Module parsing tests
  test("org:name:version") {
    dependencyParams("org.apache.avro:avro:1.7.4", scala211) match {
      case Left(err) => assert(false)
      case Right((dep, _)) =>
        assert(dep.module.organization == org"org.apache.avro")
        assert(dep.module.name == name"avro")
        assert(dep.version == "1.7.4")
        assert(dep.configuration == Configuration.empty)
        assert(dep.attributes == Attributes())
    }
  }

  test("org:name:version:config") {
    dependencyParams("org.apache.avro:avro:1.7.4:runtime", scala211) match {
      case Left(err) => assert(false)
      case Right((dep, _)) =>
        assert(dep.module.organization == org"org.apache.avro")
        assert(dep.module.name == name"avro")
        assert(dep.version == "1.7.4")
        assert(dep.configuration == Configuration.runtime)
        assert(dep.attributes == Attributes())
    }
  }

  test("org:name:interval:config") {
    dependencyParams("org.apache.avro:avro:[1.7,1.8):runtime", scala211) match {
      case Left(err) => assert(false)
      case Right((dep, _)) =>
        assert(dep.module.organization == org"org.apache.avro")
        assert(dep.module.name == name"avro")
        assert(dep.version == "[1.7,1.8)")
        assert(dep.configuration == Configuration.runtime)
        assert(dep.attributes == Attributes())
    }
  }

  test("single attr") {
    dependencyParams(
      "org.apache.avro:avro:1.7.4:runtime,classifier=tests",
      scala211
    ) match {
      case Left(err) => assert(false)
      case Right((dep, _)) =>
        assert(dep.module.organization == org"org.apache.avro")
        assert(dep.module.name == name"avro")
        assert(dep.version == "1.7.4")
        assert(dep.configuration == Configuration.runtime)
        assert(dep.attributes == Attributes(Type.empty, Classifier.tests))
    }
  }

  test("extension") {
    dependencyParams(
      "org.apache.avro:avro:1.7.4:runtime,ext=exe",
      scala211
    ) match {
      case Left(err) => assert(false)
      case Right((dep, _)) =>
        assert(dep.module.organization == org"org.apache.avro")
        assert(dep.module.name == name"avro")
        assert(dep.version == "1.7.4")
        assert(dep.configuration == Configuration.runtime)
        assert(dep.publication == Publication("", Type.empty, Extension("exe"), Classifier.empty))
    }
  }

  test("type") {
    dependencyParams(
      "org.apache.avro:avro:1.7.4:runtime,type=typetype",
      scala211
    ) match {
      case Left(err) => assert(false)
      case Right((dep, _)) =>
        assert(dep.module.organization == org"org.apache.avro")
        assert(dep.module.name == name"avro")
        assert(dep.version == "1.7.4")
        assert(dep.configuration == Configuration.runtime)
        val expectedPublication = Publication(
          "",
          Type("typetype"),
          Extension.empty,
          Classifier.empty
        )
        assert(dep.publication == expectedPublication)
    }
  }

  test("extension and type") {
    dependencyParams(
      "org.apache.avro:avro:1.7.4:runtime,ext=exe,type=typetype",
      scala211
    ) match {
      case Left(err) => assert(false)
      case Right((dep, _)) =>
        assert(dep.module.organization == org"org.apache.avro")
        assert(dep.module.name == name"avro")
        assert(dep.version == "1.7.4")
        assert(dep.configuration == Configuration.runtime)
        val expectedPublication = Publication(
          "",
          Type("typetype"),
          Extension("exe"),
          Classifier.empty
        )
        assert(dep.publication == expectedPublication)
    }
  }

  test("single attr with interval") {
    dependencyParams(
      "org.apache.avro:avro:[1.7,1.8):runtime,classifier=tests",
      scala211
    ) match {
      case Left(err) => assert(false)
      case Right((dep, _)) =>
        assert(dep.module.organization == org"org.apache.avro")
        assert(dep.module.name == name"avro")
        assert(dep.version == "[1.7,1.8)")
        assert(dep.configuration == Configuration.runtime)
        assert(dep.attributes == Attributes(Type.empty, Classifier.tests))
    }
  }

  test("single attr with url") {
    dependencyParams(
      "org.apache.avro:avro:1.7.4:runtime,url=" + url,
      scala211
    ) match {
      case Left(err) => assert(false)
      case Right((dep, extraParams)) =>
        assert(dep.module.organization == org"org.apache.avro")
        assert(dep.module.name == name"avro")
        assert(dep.version == "1.7.4")
        assert(dep.configuration == Configuration.runtime)
        assert(dep.attributes == Attributes())
        assert(extraParams.isDefinedAt("url"))
        assert(extraParams.getOrElse("url", "") == url)
    }
  }

  test("multiple attrs with url") {
    dependencyParams(
      "org.apache.avro:avro:1.7.4:runtime,classifier=tests,url=" + url,
      scala211
    ) match {
      case Left(err) => assert(false)
      case Right((dep, extraParams)) =>
        assert(dep.module.organization == org"org.apache.avro")
        assert(dep.module.name == name"avro")
        assert(dep.version == "1.7.4")
        assert(dep.configuration == Configuration.runtime)
        assert(dep.attributes == Attributes(Type.empty, Classifier.tests))
        assert(extraParams.isDefinedAt("url"))
        assert(extraParams.getOrElse("url", "") == url)
    }
  }

  test("multiple attrs with interval and url") {
    dependencyParams(
      "org.apache.avro:avro:[1.7,1.8):runtime,classifier=tests,url=" + url,
      scala211
    ) match {
      case Left(err) => assert(false)
      case Right((dep, extraParams)) =>
        assert(dep.module.organization == org"org.apache.avro")
        assert(dep.module.name == name"avro")
        assert(dep.version == "[1.7,1.8)")
        assert(dep.configuration == Configuration.runtime)
        assert(dep.attributes == Attributes(Type.empty, Classifier.tests))
        assert(extraParams.isDefinedAt("url"))
        assert(extraParams.getOrElse("url", "") == url)
    }
  }

  test("multiple attrs with interval, url, and exclusions") {
    dependencyParams(
      "org.apache.avro:avro:[1.7,1.8):runtime,classifier=tests,url=" + url + ",exclude=org%nme",
      scala211
    ) match {
      case Left(err) => assert(false)
      case Right((dep, extraParams)) =>
        assert(dep.module.organization == org"org.apache.avro")
        assert(dep.module.name == name"avro")
        assert(dep.version == "[1.7,1.8)")
        assert(dep.configuration == Configuration.runtime)
        assert(dep.attributes == Attributes(Type.empty, Classifier.tests))
        assert(extraParams.isDefinedAt("url"))
        assert(extraParams.getOrElse("url", "") == url)
        assert(dep.exclusions == Set((org"org", name"nme")))
    }
  }

  test("single attr with org::name:version") {
    dependencyParams(
      "io.get-coursier.scala-native::sandbox_native0.3:0.3.0-coursier-1,classifier=tests",
      scala211
    ) match {
      case Left(err) => assert(false)
      case Right((dep, _)) =>
        assert(dep.module.organization == org"io.get-coursier.scala-native")
        // use `contains` to be scala version agnostic
        assert(dep.module.name.value.contains("sandbox_native0.3"))
        assert(dep.version == "0.3.0-coursier-1")
        assert(dep.attributes == Attributes(Type.empty, Classifier.tests))
    }
  }

  test("single attr with org::name:interval") {
    dependencyParams(
      "io.get-coursier.scala-native::sandbox_native0.3:[0.3.0,0.4.0),classifier=tests",
      scala211
    ) match {
      case Left(err) => assert(false)
      case Right((dep, _)) =>
        assert(dep.module.organization == org"io.get-coursier.scala-native")
        // use `contains` to be scala version agnostic
        assert(dep.module.name.value.contains("sandbox_native0.3"))
        assert(dep.version == "[0.3.0,0.4.0)")
        assert(dep.attributes == Attributes(Type.empty, Classifier.tests))
    }
  }

  test("multiple attr with org::name:interval and exclusion") {
    dependencyParams(
      "io.get-coursier.scala-native::sandbox_native0.3:[0.3.0,0.4.0),classifier=tests,exclude=foo%bar",
      scala211
    ) match {
      case Left(err) => assert(false)
      case Right((dep, _)) =>
        assert(dep.module.organization == org"io.get-coursier.scala-native")
        // use `contains` to be scala version agnostic
        assert(dep.module.name.value.contains("sandbox_native0.3"))
        assert(dep.version == "[0.3.0,0.4.0)")
        assert(dep.attributes == Attributes(Type.empty, Classifier.tests))
        assert(dep.exclusions == Set((org"foo", name"bar")))
    }
  }

  test("full cross versioned org:::name:version") {
    dependencyParams("com.lihaoyi:::ammonite:1.6.7", scala212) match {
      case Left(err) => assert(false)
      case Right((dep, _)) =>
        assert(dep.module.organization == org"com.lihaoyi")
        assert(dep.module.name.value == "ammonite_2.12.8")
        assert(dep.version == "1.6.7")
    }
  }

  test("full cross versioned org:::name:version with exclusion") {
    dependencyParams(
      "com.lihaoyi:::ammonite:1.6.7,exclude=aa%*",
      scala212
    ) match {
      case Left(err) => assert(false)
      case Right((dep, _)) =>
        assert(dep.module.organization == org"com.lihaoyi")
        assert(dep.module.name.value == "ammonite_2.12.8")
        assert(dep.version == "1.6.7")
        assert(dep.exclusions == Set((org"aa", name"*")))
    }
  }

  test("illegal 1") {
    dependencyParams("junit:junit:4.12,attr", scala211) match {
      case Left(err)  => assert(err.contains("Failed to parse attribute"))
      case Right(dep) => assert(false)
    }
  }

  test("illegal 2") {
    dependencyParams("a:b:c,batman=robin", scala211) match {
      case Left(err)  => assert(err.contains("The only attributes allowed are:"))
      case Right(dep) => assert(false)
    }
  }

  test("illegal 3, malformed exclude") {
    dependencyParams("a:b:c,exclude=aaa", scala211) match {
      case Left(err)  => assert(err.contains("Malformed exclusion:"))
      case Right(dep) => assert(false)
    }
  }

  test("scala module") {
    DependencyParser.javaOrScalaDependencyParams("org::name:ver", Configuration.empty) match {
      case Left(err) => sys.error(err)
      case Right((dep, params)) =>
        assert(params.isEmpty)
        val expected = JavaOrScalaDependency.ScalaDependency(
          Dependency(mod"org:name", "ver").withConfiguration(Configuration.empty),
          fullCrossVersion = false,
          withPlatformSuffix = false,
          exclude = Set.empty
        )
        assert(dep == expected)
    }
  }

  test("full cross versioned scala module") {
    DependencyParser.javaOrScalaDependencyParams("org:::name:ver", Configuration.empty) match {
      case Left(err) => sys.error(err)
      case Right((dep, params)) =>
        assert(params.isEmpty)
        val expected = JavaOrScalaDependency.ScalaDependency(
          Dependency(mod"org:name", "ver").withConfiguration(Configuration.empty),
          fullCrossVersion = true,
          withPlatformSuffix = false,
          exclude = Set.empty
        )
        assert(dep == expected)
    }
  }

  test("full cross versioned scala module with config") {
    DependencyParser.javaOrScalaDependencyParams("org:::name:ver:conf", Configuration.empty) match {
      case Left(err) => sys.error(err)
      case Right((dep, params)) =>
        assert(params.isEmpty)
        val expected = JavaOrScalaDependency.ScalaDependency(
          Dependency(mod"org:name", "ver").withConfiguration(Configuration("conf")),
          fullCrossVersion = true,
          withPlatformSuffix = false,
          exclude = Set.empty
        )
        assert(dep == expected)
    }
  }
}
