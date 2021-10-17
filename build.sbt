lazy val bleep = project
  .in(file("."))
  .enablePlugins(GitVersioning, NativeImagePlugin, TpolecatPlugin)
  .settings(
    name := "bleep",
    organization := "no.arktekk",
    scalaVersion := "2.13.6",
    scalacOptions -= "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.1",
      "io.circe" %% "circe-parser" % "0.14.1",
      "io.circe" %% "circe-generic" % "0.14.1",
      "ch.epfl.scala" %% "bloop-config" % "1.4.9",
      "io.get-coursier" %% "coursier" % "2.0.16",
      "org.scalatest" %% "scalatest" % "3.2.10" % Test,
      "org.scalameta" % "svm-subs" % "101.0.0"
    ),
    Compile / mainClass := Some("bleep.Main")
  )

lazy val infrastructure = project
  .enablePlugins(TpolecatPlugin)
  .settings(
    scalaVersion := "2.13.6",
    scalacOptions -= "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "ch.epfl.scala" %% "bloop-config" % "1.4.9",
      "org.scalameta" % "svm-subs" % "101.0.0"
    )
  )
