package bleep

import bleep.commands.Import
import org.scalactic.TripleEqualsSupport
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Path

class SbtOutputParserTest extends AnyFunSuite with TripleEqualsSupport {
  test("parse project output") {
    val input = """[info] welcome to sbt 1.6.2 (AdoptOpenJDK Java 1.8.0_292)
                  |[info] loading global plugins from /home/foo/.sbt/1.0/plugins
                  |[info] loading settings for project bloop-build-build-build from build.sbt ...
                  |[info] loading project definition from /home/foo/bleep/snapshot-tests-in/bloop/project/project/project
                  |[info] loading settings for project root from build.sbt ...
                  |[info] loading project definition from /home/foo/bleep/snapshot-tests-in/bloop/project/project
                  |[info] Wrote /home/foo/bleep/snapshot-tests-in/bloop/project/project/sbt-bloop-build-shaded-naked/target/scala-2.12/sbt-bloop-build-shaded-naked_2.12-1.0.0-SNAPSHOT.pom
                  |[info] Found 504 class(es) in JAR(s) to be shaded
                  |[info] 0 remaining class(es) to be shaded
                  |[info] :: delivering :: ch.epfl.scala#sbt-bloop-build-shaded-naked_2.12;1.0.0-SNAPSHOT :: 1.0.0-SNAPSHOT :: integration :: Thu Jul 21 23:50:30 CEST 2022
                  |[info] 	delivering ivy file to /home/foo/bleep/snapshot-tests-in/bloop/project/project/sbt-bloop-build-shaded-naked/target/scala-2.12/ivy-1.0.0-SNAPSHOT.xml
                  |[info] 	published sbt-bloop-build-shaded-naked_2.12 to /home/foo/.ivy2/local/ch.epfl.scala/sbt-bloop-build-shaded-naked_2.12/1.0.0-SNAPSHOT/poms/sbt-bloop-build-shaded-naked_2.12.pom
                  |[info] 	published sbt-bloop-build-shaded-naked_2.12 to /home/foo/.ivy2/local/ch.epfl.scala/sbt-bloop-build-shaded-naked_2.12/1.0.0-SNAPSHOT/jars/sbt-bloop-build-shaded-naked_2.12.jar
                  |[info] 	published ivy to /home/foo/.ivy2/local/ch.epfl.scala/sbt-bloop-build-shaded-naked_2.12/1.0.0-SNAPSHOT/ivys/ivy.xml
                  |[info] Wrote /home/foo/bleep/snapshot-tests-in/bloop/project/project/target/sbt-bloop-build-shaded/target/scala-2.12/sbt-1.0/sbt-bloop-build-shaded-1.0.0-SNAPSHOT.pom
                  |[info] :: delivering :: ch.epfl.scala#sbt-bloop-build-shaded;1.0.0-SNAPSHOT :: 1.0.0-SNAPSHOT :: integration :: Thu Jul 21 23:50:30 CEST 2022
                  |[info] 	delivering ivy file to /home/foo/bleep/snapshot-tests-in/bloop/project/project/target/sbt-bloop-build-shaded/target/scala-2.12/sbt-1.0/ivy-1.0.0-SNAPSHOT.xml
                  |[info] 	published sbt-bloop-build-shaded to /home/foo/.ivy2/local/ch.epfl.scala/sbt-bloop-build-shaded/scala_2.12/sbt_1.0/1.0.0-SNAPSHOT/poms/sbt-bloop-build-shaded.pom
                  |[info] 	published sbt-bloop-build-shaded to /home/foo/.ivy2/local/ch.epfl.scala/sbt-bloop-build-shaded/scala_2.12/sbt_1.0/1.0.0-SNAPSHOT/jars/sbt-bloop-build-shaded.jar
                  |[info] 	published ivy to /home/foo/.ivy2/local/ch.epfl.scala/sbt-bloop-build-shaded/scala_2.12/sbt_1.0/1.0.0-SNAPSHOT/ivys/ivy.xml
                  |[info] compiling 1 Scala source to /home/foo/bleep/snapshot-tests-in/bloop/project/project/target/scala-2.12/sbt-1.0/classes ...
                  |[info] loading settings for project bloop-build from build.sbt,plugins.sbt ...
                  |[info] loading project definition from /home/foo/bleep/snapshot-tests-in/bloop/project
                  |[success] Generated .bloop/bloop-shaded-plugin.json
                  |[success] Generated .bloop/bloop-build.json
                  |[success] Total time: 7 s, completed Jul 21, 2022 11:50:53 PM
                  |/home/foo/bleep/snapshot-tests-in/bloop/build.sbt:5: warning: method useGpg in object autoImport is deprecated (since 2.0.0): useGpg is true by default; Bouncy Castle mode is deprecated
                  |Global / useGpg := false
                  |         ^
                  |[info] loading settings for project bloop from build.sbt ...
                  |[info] loading settings for project benchmark-bridge-build from plugins.sbt ...
                  |[info] loading project definition from /home/foo/bleep/snapshot-tests-in/bloop/benchmark-bridge/project
                  |[info] loading settings for project benchmark-bridge from build.sbt ...
                  |[info] resolving key references (49790 settings) ...
                  |[info]       _____            __         ______           __
                  |[info]      / ___/_________ _/ /___ _   / ____/__  ____  / /____  _____
                  |[info]      \__ \/ ___/ __ `/ / __ `/  / /   / _ \/ __ \/ __/ _ \/ ___/
                  |[info]     ___/ / /__/ /_/ / / /_/ /  / /___/ /__/ / / / /_/ /__/ /
                  |[info]    /____/\___/\__,_/_/\__,_/   \____/\___/_/ /_/\__/\___/_/
                  |[info] 
                  |[info]    ***********************************************************
                  |[info]    ***       Welcome to the build of `loooooooooop`        ***
                  |[info]    ***        An effort funded by the Scala Center         ***
                  |[info]    ***********************************************************
                  |[info]     
                  |[warn] there are 2 keys that are not used by any other settings/tasks:
                  |[warn]  
                  |[warn] * frontend / Compile / run / bloopMainClass
                  |[warn]   +- /home/foo/bleep/snapshot-tests-in/bloop/build.sbt:315
                  |[warn] * frontend / bloopName
                  |[warn]   +- /home/foo/bleep/snapshot-tests-in/bloop/build.sbt:313
                  |[warn]  
                  |[warn] note: a setting might still be used by a command; to exclude a key from this `lintUnused` check
                  |[warn] either append it to `Global / excludeLintKeys` or call .withRank(KeyRanks.Invisible) on the key
                  |[info] sbt server started at local:///home/foo/.sbt/1.0/server/4e0ccdc84064332d2c06/sock
                  |[info] started sbt server
                  |[info] In file:/home/foo/bleep/snapshot-tests-in/bloop/
                  |[info] 	   backend
                  |[info] 	   benchmarkBridge
                  |[info] 	   benchmarks
                  |[info] 	 * bloop
                  |[info] 	   bloop4j
                  |[info] 	   bloopShared
                  |[info] In file:///home/foo/bleep/snapshot-tests-in/bloop/benchmark-bridge/
                  |[info] 	   jvm
                  |[info] 	   micro""".stripMargin

    val result = Import.parseProjectsOutput(input.split("\n"))

    val expected = Map(
      Path.of("/home/foo/bleep/snapshot-tests-in/bloop") -> List("backend", "benchmarkBridge", "benchmarks", "bloop", "bloop4j", "bloopShared"),
      Path.of("/home/foo/bleep/snapshot-tests-in/bloop/benchmark-bridge") -> List("jvm", "micro")
    )

    assert(result === expected)
  }

  test("parse scala version output") {
    val output = """[info] ...
                   |[info] binaryJVMProjects / scalaVersion
                   |[info] 	2.12.16
                   |[info] ...
                   |[info] treesNative / crossScalaVersions
                   |[info] 	List(2.13.8, 2.12.16)
                   |[info] ProjectRef(uri("file:///home/foo/bleep/snapshot-tests-in/bloop/benchmark-bridge/"), "compilation") / crossScalaVersions
                   |[info]  List(2.12.16)
                   |[info] treesNative2 / crossScalaVersions
                   |[info] 	List(2.12.16)
                   |[info] ...""".stripMargin

    val result = Import.ScalaVersionOutput.parse(output.split("\n"), None).combined
    val expected =
      Map(model.VersionScala("2.13.8") -> Set("treesNative"), model.VersionScala("2.12.16") -> Set("treesNative2", "treesNative", "binaryJVMProjects"))
    assert(result === expected)
  }

  test("parse short scala version output") {
    val projectName = "test12"
    val output = """[info] scalaVersion
                   |[info] 	3.2.0
                   |[info] crossScalaVersions
                   |[info] 	List(3.2.0)""".stripMargin

    val result = Import.ScalaVersionOutput.parse(output.split("\n"), Some(projectName)).combined
    val expected = Map(model.VersionScala("3.2.0") -> Set(projectName))
    assert(result === expected)
  }
}
