val commonSettings: Project => Project =
  _.enablePlugins(GitVersioning, TpolecatPlugin)
    .settings(
      organization := "no.arktekk",
      scalaVersion := "2.13.6",
      scalacOptions -= "-Xfatal-warnings"
    )

lazy val `bleep-core` = project
  .configure(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "io.get-coursier" %% "coursier" % "2.0.16",
      "io.circe" %% "circe-core" % "0.14.1",
      "io.circe" %% "circe-parser" % "0.14.1",
      "io.circe" %% "circe-generic" % "0.14.1",
      "ch.epfl.scala" %% "bloop-config" % "1.4.9",
      "net.harawata" % "appdirs" % "1.2.1"
    )
  )

lazy val `bleep-tasks` = project
  .configure(commonSettings)
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
  .enablePlugins(NativeImagePlugin)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.10" % Test,
      "org.scalameta" % "svm-subs" % "101.0.0",
      "com.monovore" %% "decline-effect" % "2.1.0"
    ),
    Compile / mainClass := Some("bleep.Main")
  )

lazy val infrastructure = project
  .dependsOn(`bleep-tasks`)
  .configure(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "ch.epfl.scala" %% "bloop-config" % "1.4.9"
    )
  )
