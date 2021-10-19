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
      "ch.epfl.scala" %% "bloop-config" % "1.4.9"
    )
  )

lazy val `bleep-tasks` = project
  .configure(commonSettings)
  .dependsOn(`bleep-core`)

lazy val bleep = project
  .configure(commonSettings)
  .dependsOn(`bleep-core`)
  .enablePlugins(NativeImagePlugin)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.10" % Test,
      "org.scalameta" % "svm-subs" % "101.0.0"
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
