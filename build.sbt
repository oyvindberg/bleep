import sbt.librarymanagement.For3Use2_13

name := "bleep-root"
crossScalaVersions := List("2.12.15", "2.13.8", "3.1.1")

val commonSettings: Project => Project =
  _.enablePlugins(GitVersioning, TpolecatPlugin)
    .settings(
      organization := "no.arktekk",
      scalaVersion := "2.13.8",
      scalacOptions -= "-Xfatal-warnings"
    )

val crossSettings: Project => Project =
  _.settings(
    crossScalaVersions := List("2.12.15", "2.13.8", "3.1.1")
  )

lazy val `bleep-core` = project
  .configure(commonSettings, crossSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fansi" % "0.3.1",
      "io.get-coursier" %% "coursier" % "2.0.16" cross For3Use2_13() exclude ("org.scala-lang.modules", "scala-collection-compat_2.13"),
      "io.circe" %% "circe-core" % "0.14.1",
      "io.circe" %% "circe-parser" % "0.14.1",
      "io.circe" %% "circe-generic" % "0.14.1",
      ("org.gnieh" %% "diffson-circe" % "4.1.1"),
      "ch.epfl.scala" %% "bloop-config" % "1.4.13" cross For3Use2_13()
    )
  )

lazy val `bleep-tasks` = project
  .configure(commonSettings, crossSettings)
  .dependsOn(`bleep-core`)
  .settings(
    libraryDependencies ++= List(
      "se.sawano.java" % "alphanumeric-comparator" % "1.4.1"
    ),
    Compile / unmanagedSourceDirectories ++= List(
      baseDirectory.value / "liberated/sbt-native-image/plugin/src/main/scala",
      baseDirectory.value / "liberated/sbt-git-versioning/src/main/scala",
      baseDirectory.value / "liberated/bloop-packager/src/main/scala"
    ),
    Compile / unmanagedResourceDirectories ++= List(
      baseDirectory.value / "liberated/sbt-native-image/plugin/src/main/resources"
    )
  )

lazy val bleep = project
  .configure(commonSettings)
  .dependsOn(`bleep-core`)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.11" % Test,
      "org.scalameta" % "svm-subs" % "101.0.0",
      "com.monovore" %% "decline" % "2.2.0",
      "com.lihaoyi" %% "pprint" % "0.7.3",
      "org.virtuslab.scala-cli" %% "bloop-rifle" % "0.1.4",
      "org.graalvm.nativeimage" % "svm" % "22.0.0.2",
      ("org.scala-sbt" %% "librarymanagement-core" % "1.6.0").exclude("org.scala-sbt", "util-logging_2.13")
    ),
    Compile / bloopMainClass := Some("bleep.Main"),
    nativeImageJvmIndex := "https://raw.githubusercontent.com/coursier/jvm-index/master/index.json",
    nativeImageJvm := "graalvm-java17",
    nativeImageVersion := "22.0.0.2",
    assemblyMergeStrategy := {
      case PathList(ps @ _*) if ps.last.endsWith("module-info.class") => MergeStrategy.discard
      case x =>
        val oldStrategy = (ThisBuild / assemblyMergeStrategy).value
        oldStrategy(x)
    },
    assemblyJarName := "bleep-assembly.jar"
  )
  .enablePlugins(NativeImagePlugin)

lazy val scripts = project
  .dependsOn(bleep, `bleep-tasks`)
  .configure(commonSettings)
