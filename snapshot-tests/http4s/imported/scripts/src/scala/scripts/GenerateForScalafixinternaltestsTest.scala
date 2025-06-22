
package scripts

import bleep.{BleepCodegenScript, Commands, Started}

import java.nio.file.Files

object GenerateForScalafixinternaltestsTest extends BleepCodegenScript("GenerateForScalafixinternaltestsTest") {
  override def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit = {
    started.logger.error("This script is a placeholder! You'll need to replace the contents with code which actually generates the files you want")

    targets.foreach { target =>
      if (Set("scalafixinternaltests-test@jvm3").contains(target.project.value)) {
        val to = target.resources.resolve("scalafix-testkit.properties")
        started.logger.withContext(target.project).warn(s"Writing $to")
        val content = s"""|#Input data for scalafix testkit
      |#Sun Jun 22 00:34:40 CEST 2025
      |inputClasspath=<BLEEP_GIT>/snapshot-tests/http4s/sbt-build/scalafix-internal/input/target/scala-3.3.4/classes\\\\:<BLEEP_GIT>/snapshot-tests/http4s/sbt-build/core/jvm/target/scala-3.3.4/classes\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.3.4/scala3-library_3-3.3.4.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/scalac-compat-annotation_3/0.1.4/scalac-compat-annotation_3-0.1.4.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/case-insensitive_3/1.4.2/case-insensitive_3-1.4.2.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/cats-core_3/2.11.0/cats-core_3-2.11.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/cats-effect-std_3/3.5.7/cats-effect-std_3-3.5.7.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/cats-parse_3/1.0.0/cats-parse_3-1.0.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/co/fs2/fs2-core_3/3.11.0/fs2-core_3-3.11.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/co/fs2/fs2-io_3/3.11.0/fs2-io_3-3.11.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/com/comcast/ip4s-core_3/3.6.0/ip4s-core_3-3.6.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/literally_3/1.1.0/literally_3-1.1.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/log4cats-core_3/2.7.0/log4cats-core_3-2.7.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/scodec/scodec-bits_3/1.1.38/scodec-bits_3-1.1.38.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/vault_3/3.6.0/vault_3-3.6.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.14/scala-library-2.13.14.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/cats-kernel_3/2.11.0/cats-kernel_3-2.11.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/cats-effect-kernel_3/3.5.7/cats-effect-kernel_3-3.5.7.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/cats-effect_3/3.5.4/cats-effect_3-3.5.4.jar
      |inputSourceDirectories=<BLEEP_GIT>/snapshot-tests/http4s/sbt-build/scalafix-internal/input/src/main/scala
      |outputSourceDirectories=<BLEEP_GIT>/snapshot-tests/http4s/sbt-build/scalafix-internal/output/src/main/scala
      |scalaVersion=3.3.4
      |scalacOptions=-deprecation|-encoding|UTF-8|-feature|-unchecked|-Wunused\\\\:implicits|-Wunused\\\\:explicits|-Wunused\\\\:imports|-Wunused\\\\:locals|-Wunused\\\\:params|-Wunused\\\\:privates|-Wvalue-discard|-language\\\\:implicitConversions|-Ykind-projector|-java-output-version|8|-Xsemanticdb|-semanticdb-target|<BLEEP_GIT>/snapshot-tests/http4s/sbt-build/scalafix-internal/input/target/scala-3.3.4/meta
      |sourceroot=<BLEEP_GIT>/snapshot-tests/http4s/sbt-build""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set("scalafixinternaltests-test@jvm213").contains(target.project.value)) {
        val to = target.resources.resolve("scalafix-testkit.properties")
        started.logger.withContext(target.project).warn(s"Writing $to")
        val content = s"""|#Input data for scalafix testkit
      |#Sun Jun 22 00:33:31 CEST 2025
      |inputClasspath=<BLEEP_GIT>/snapshot-tests/http4s/sbt-build/scalafix-internal/input/target/scala-2.13/classes\\\\:<BLEEP_GIT>/snapshot-tests/http4s/sbt-build/core/jvm/target/scala-2.13/classes\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.15/scala-library-2.13.15.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/scalac-compat-annotation_2.13/0.1.4/scalac-compat-annotation_2.13-0.1.4.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/case-insensitive_2.13/1.4.2/case-insensitive_2.13-1.4.2.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/cats-core_2.13/2.11.0/cats-core_2.13-2.11.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/cats-effect-std_2.13/3.5.7/cats-effect-std_2.13-3.5.7.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/cats-parse_2.13/1.0.0/cats-parse_2.13-1.0.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/co/fs2/fs2-core_2.13/3.11.0/fs2-core_2.13-3.11.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/co/fs2/fs2-io_2.13/3.11.0/fs2-io_2.13-3.11.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/com/comcast/ip4s-core_2.13/3.6.0/ip4s-core_2.13-3.6.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/literally_2.13/1.1.0/literally_2.13-1.1.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/log4cats-core_2.13/2.7.0/log4cats-core_2.13-2.7.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/scodec/scodec-bits_2.13/1.1.38/scodec-bits_2.13-1.1.38.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/vault_2.13/3.6.0/vault_2.13-3.6.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/cats-kernel_2.13/2.11.0/cats-kernel_2.13-2.11.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/cats-effect-kernel_2.13/3.5.7/cats-effect-kernel_2.13-3.5.7.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/scala-lang/scala-reflect/2.13.15/scala-reflect-2.13.15.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/cats-effect_2.13/3.5.4/cats-effect_2.13-3.5.4.jar
      |inputSourceDirectories=<BLEEP_GIT>/snapshot-tests/http4s/sbt-build/scalafix-internal/input/src/main/scala
      |outputSourceDirectories=<BLEEP_GIT>/snapshot-tests/http4s/sbt-build/scalafix-internal/output/src/main/scala
      |scalaVersion=2.13.15
      |scalacOptions=-deprecation|-encoding|UTF-8|-feature|-unchecked|-Wdead-code|-Wextra-implicit|-Wnumeric-widen|-Wunused|-Wvalue-discard|-Xlint\\\\:_,-implicit-recursion,-recurse-with-default,-unused,-byname-implicit|-Wconf\\\\:cat\\\\=scala3-migration\\\\:s|-Ybackend-parallelism|10|-language\\\\:_|-Xsource\\\\:3|-release|8|-Xplugin\\\\:<COURSIER>/https/repo1.maven.org/maven2/com/olegpy/better-monadic-for_2.13/0.3.1/better-monadic-for_2.13-0.3.1.jar|-Xplugin\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/kind-projector_2.13.15/0.13.3/kind-projector_2.13.15-0.13.3.jar|-Xplugin\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/scalameta/semanticdb-scalac_2.13.15/4.10.1/semanticdb-scalac_2.13.15-4.10.1.jar|-Yrangepos|-P\\\\:semanticdb\\\\:synthetics\\\\:on|-P\\\\:semanticdb\\\\:targetroot\\\\:<BLEEP_GIT>/snapshot-tests/http4s/sbt-build/scalafix-internal/input/target/scala-2.13/meta
      |sourceroot=<BLEEP_GIT>/snapshot-tests/http4s/sbt-build""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }

  }
}