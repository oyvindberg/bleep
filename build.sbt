import sbt.librarymanagement.For3Use2_13

val scala212 = "2.12.15"
val scala213 = "2.13.8"
val scala3 = "3.1.1"
val scalaNot3 = List(scala212, scala213)
val scalaAll = scalaNot3 ++ List(scala3)

// root project config
name := "bleep-root"
publish / skip := true

val commonSettings: Project => Project =
  _.enablePlugins(TpolecatPlugin)
    .settings(
      organization := "build.bleep",
      scalacOptions -= "-Xfatal-warnings",
      dynverSonatypeSnapshots := true,
      homepage := Some(url("https://github.com/oyvindberg/bleep/")),
      licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
      developers := List(
        Developer(
          "oyvindberg",
          "Øyvind Raddum Berg",
          "elacin@gmail.com",
          url("https://github.com/oyvindberg")
        ),
        Developer(
          "hamnis",
          "Erlend Hamnaberg",
          "erlend@hamnaberg.net",
          url("https://github.com/hamnis")
        )
      ),
      sonatypeCredentialHost := "s01.oss.sonatype.org",
      Compile / doc / sources := Nil,
      evictionErrorLevel := Level.Warn
    )

lazy val `bleep-core` = projectMatrix
  .configure(commonSettings)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fansi" % "0.3.1",
      "io.get-coursier" %% "coursier" % "2.1.0-M6" cross For3Use2_13() exclude ("org.scala-lang.modules", "scala-collection-compat_2.13"),
      "io.get-coursier" %% "coursier-jvm" % "2.1.0-M6" cross For3Use2_13() exclude ("org.scala-lang.modules", "scala-collection-compat_2.13"),
      "io.circe" %% "circe-core" % "0.14.2",
      "io.circe" %% "circe-parser" % "0.14.2",
      "io.circe" %% "circe-generic" % "0.14.2",
      "io.circe" %% "circe-yaml" % "0.14.1",
      "org.yaml" % "snakeyaml" % "1.30",
      ("org.gnieh" %% "diffson-circe" % "4.1.1"),
      "ch.epfl.scala" %% "bloop-config" % "1.5.0" cross For3Use2_13(),
      "org.virtuslab.scala-cli" %% "bloop-rifle" % "0.1.6"
    ),
    buildInfoObject := "BleepVersion",
    buildInfoKeys := List(version),
    buildInfoPackage := "bleep"
  )
  .jvmPlatform(scalaAll)

lazy val `bleep-test-harness` = projectMatrix
  .configure(commonSettings)
  .dependsOn(`bleep-core`)
  .settings(
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12"
  )
  .jvmPlatform(scalaNot3)

lazy val `bleep-tasks` = projectMatrix
  .configure(commonSettings)
  .dependsOn(`bleep-core`)
  .settings(
    Compile / unmanagedSourceDirectories ++= List(
      (ThisBuild / baseDirectory).value / "liberated/sbt-native-image/plugin/src/main/scala"
    ),
    Compile / unmanagedResourceDirectories ++= List(
      (ThisBuild / baseDirectory).value / "liberated/sbt-native-image/plugin/src/main/resources"
    )
  )
  .jvmPlatform(scalaAll)

lazy val `bleep-tasks-publishing` = projectMatrix
  .configure(commonSettings)
  .dependsOn(`bleep-tasks`, `bleep-test-harness` % Test)
  .settings(
    libraryDependencies ++= List(
      "se.sawano.java" % "alphanumeric-comparator" % "1.4.1",
      "org.sonatype.spice.zapper" % "spice-zapper" % "1.3",
      "org.wvlet.airframe" %% "airframe-http" % "22.4.2",
      "com.eed3si9n" %% "gigahorse-okhttp" % "0.6.0",
      "org.bouncycastle" % "bcpg-jdk15on" % "1.70",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"
    ),
    Compile / unmanagedSourceDirectories ++= List(
      (ThisBuild / baseDirectory).value / "liberated/sbt-git-versioning/src/main/scala",
      (ThisBuild / baseDirectory).value / "liberated/sbt-sonatype/src/main/scala",
      (ThisBuild / baseDirectory).value / "liberated/sbt-pgp/gpg-library/src/main/scala",
      (ThisBuild / baseDirectory).value / "liberated/sbt-pgp/sbt-pgp/src/main/scala",
      (ThisBuild / baseDirectory).value / "liberated/sbt-ci-release/plugin/src/main/scala",
      (ThisBuild / baseDirectory).value / "liberated/sbt-dynver/dynver/src/main/scala",
      (ThisBuild / baseDirectory).value / "liberated/sbt-dynver/sbtdynver/src/main/scala"
    )
  )
  .jvmPlatform(scalaNot3)

lazy val `bleep-cli` = projectMatrix
  .configure(commonSettings)
  .dependsOn(`bleep-core`, `bleep-test-harness` % Test)
  .settings(
    publish / skip := true,
    libraryDependencies ++= Seq(
      "org.scalameta" % "svm-subs" % "101.0.0",
      "com.monovore" %% "decline" % "2.2.0",
      "com.lihaoyi" %% "pprint" % "0.7.3",
      "org.graalvm.nativeimage" % "svm" % "22.1.0.1",
      ("org.scala-sbt" %% "librarymanagement-core" % "1.6.1").exclude("org.scala-sbt", "util-logging_2.13")
    ),
    Compile / bloopMainClass := Some("bleep.Main"),
    nativeImageJvmIndex := "https://raw.githubusercontent.com/coursier/jvm-index/master/index.json",
    nativeImageJvm := "graalvm-java17",
    nativeImageVersion := "22.1.0",
    assemblyMergeStrategy := {
      case PathList(ps @ _*) if ps.last.endsWith("module-info.class") => MergeStrategy.discard
      case x =>
        val oldStrategy = (ThisBuild / assemblyMergeStrategy).value
        oldStrategy(x)
    },
    assemblyJarName := "bleep-cli-assembly.jar"
  )
  .enablePlugins(NativeImagePlugin)
  .jvmPlatform(List(scala213))

lazy val scripts = projectMatrix
  .configure(commonSettings)
  .settings(
    libraryDependencies ++= List("build.bleep" %% "bleep-tasks-publishing" % "0.0.1-M3"),
    publish / skip := true
  )
  .jvmPlatform(List(scala213))
