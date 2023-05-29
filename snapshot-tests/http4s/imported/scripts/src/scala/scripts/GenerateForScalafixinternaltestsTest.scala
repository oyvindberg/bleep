
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
      |#Mon May 29 12:34:30 CEST 2023
      |inputSourceDirectories=<BLEEP_GIT>/snapshot-tests/http4s/sbt-build/scalafix-internal/input/src/main/scala
      |scalaVersion=3.1.1
      |scalacOptions=-deprecation|-encoding|UTF-8|-feature|-unchecked|-language\\\\:implicitConversions|-Ykind-projector|-source\\\\:3.0-migration|-Xsemanticdb|-semanticdb-target|<BLEEP_GIT>/snapshot-tests/http4s/sbt-build/scalafix-internal/input/target/scala-3.1.1/meta
      |sourceroot=<BLEEP_GIT>/snapshot-tests/http4s/sbt-build
      |inputClasspath=<BLEEP_GIT>/snapshot-tests/http4s/sbt-build/scalafix-internal/input/target/scala-3.1.1/classes\\\\:<BLEEP_GIT>/snapshot-tests/http4s/sbt-build/core/jvm/target/scala-3.1.1/classes\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.1.1/scala3-library_3-3.1.1.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/case-insensitive_3/1.2.0/case-insensitive_3-1.2.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/cats-core_3/2.7.0/cats-core_3-2.7.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/cats-effect-std_3/3.3.6/cats-effect-std_3-3.3.6.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/cats-parse_3/0.3.6/cats-parse_3-0.3.6.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/http4s/http4s-crypto_3/0.2.2/http4s-crypto_3-0.2.2.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/co/fs2/fs2-core_3/3.2.5/fs2-core_3-3.2.5.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/co/fs2/fs2-io_3/3.2.5/fs2-io_3-3.2.5.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/com/comcast/ip4s-core_3/3.1.2/ip4s-core_3-3.1.2.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/literally_3/1.0.2/literally_3-1.0.2.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/log4s/log4s_3/1.10.0/log4s_3-1.10.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/scodec/scodec-bits_3/1.1.30/scodec-bits_3-1.1.30.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/vault_3/3.1.0/vault_3-3.1.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.6/scala-library-2.13.6.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/cats-kernel_3/2.7.0/cats-kernel_3-2.7.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/simulacrum-scalafix-annotations_3/0.5.4/simulacrum-scalafix-annotations_3-0.5.4.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/cats-effect-kernel_3/3.3.6/cats-effect-kernel_3-3.3.6.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/cats-effect_3/3.3.5/cats-effect_3-3.3.5.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/slf4j/slf4j-api/1.7.30/slf4j-api-1.7.30.jar
      |outputSourceDirectories=<BLEEP_GIT>/snapshot-tests/http4s/sbt-build/scalafix-internal/output/src/main/scala""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set("scalafixinternaltests-test@jvm213").contains(target.project.value)) {
        val to = target.resources.resolve("scalafix-testkit.properties")
        started.logger.withContext(target.project).warn(s"Writing $to")
        val content = s"""|#Input data for scalafix testkit
      |#Mon May 29 12:33:03 CEST 2023
      |inputSourceDirectories=<BLEEP_GIT>/snapshot-tests/http4s/sbt-build/scalafix-internal/input/src/main/scala
      |scalaVersion=2.13.8
      |scalacOptions=-deprecation|-encoding|UTF-8|-feature|-unchecked|-Ywarn-numeric-widen|-Xlint\\\\:deprecation|-Wunused\\\\:nowarn|-Wdead-code|-Wextra-implicit|-Wnumeric-widen|-Wunused\\\\:implicits|-Wunused\\\\:explicits|-Wunused\\\\:imports|-Wunused\\\\:locals|-Wunused\\\\:params|-Wunused\\\\:patvars|-Wunused\\\\:privates|-Wvalue-discard|-Ywarn-dead-code|-Ybackend-parallelism|10|-language\\\\:_|-Xplugin\\\\:<COURSIER>/https/repo1.maven.org/maven2/com/olegpy/better-monadic-for_2.13/0.3.1/better-monadic-for_2.13-0.3.1.jar|-Xplugin\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/kind-projector_2.13.8/0.13.2/kind-projector_2.13.8-0.13.2.jar|-Xplugin\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/scalameta/semanticdb-scalac_2.13.8/4.4.32/semanticdb-scalac_2.13.8-4.4.32.jar|-P\\\\:semanticdb\\\\:synthetics\\\\:on|-Yrangepos|-P\\\\:semanticdb\\\\:targetroot\\\\:<BLEEP_GIT>/snapshot-tests/http4s/sbt-build/scalafix-internal/input/target/scala-2.13/meta
      |sourceroot=<BLEEP_GIT>/snapshot-tests/http4s/sbt-build
      |inputClasspath=<BLEEP_GIT>/snapshot-tests/http4s/sbt-build/scalafix-internal/input/target/scala-2.13/classes\\\\:<BLEEP_GIT>/snapshot-tests/http4s/sbt-build/core/jvm/target/scala-2.13/classes\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.8/scala-library-2.13.8.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/case-insensitive_2.13/1.2.0/case-insensitive_2.13-1.2.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/cats-core_2.13/2.7.0/cats-core_2.13-2.7.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/cats-effect-std_2.13/3.3.6/cats-effect-std_2.13-3.3.6.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/cats-parse_2.13/0.3.6/cats-parse_2.13-0.3.6.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/http4s/http4s-crypto_2.13/0.2.2/http4s-crypto_2.13-0.2.2.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/co/fs2/fs2-core_2.13/3.2.5/fs2-core_2.13-3.2.5.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/co/fs2/fs2-io_2.13/3.2.5/fs2-io_2.13-3.2.5.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/com/comcast/ip4s-core_2.13/3.1.2/ip4s-core_2.13-3.1.2.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/literally_2.13/1.0.2/literally_2.13-1.0.2.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/log4s/log4s_2.13/1.10.0/log4s_2.13-1.10.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/scodec/scodec-bits_2.13/1.1.30/scodec-bits_2.13-1.1.30.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/vault_2.13/3.1.0/vault_2.13-3.1.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/slf4j/slf4j-api/1.7.36/slf4j-api-1.7.36.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/cats-kernel_2.13/2.7.0/cats-kernel_2.13-2.7.0.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/simulacrum-scalafix-annotations_2.13/0.5.4/simulacrum-scalafix-annotations_2.13-0.5.4.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/cats-effect-kernel_2.13/3.3.6/cats-effect-kernel_2.13-3.3.6.jar\\\\:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/cats-effect_2.13/3.3.5/cats-effect_2.13-3.3.5.jar
      |outputSourceDirectories=<BLEEP_GIT>/snapshot-tests/http4s/sbt-build/scalafix-internal/output/src/main/scala""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }

  }
}