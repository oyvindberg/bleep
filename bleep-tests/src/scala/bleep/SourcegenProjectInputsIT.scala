package bleep

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** `sourcegen: { inputs: [...] }`: projects whose output a generator reads. They are built before it runs and a change to their classes re-runs it, without
  * reaching the consumer's classpath — here `a` has no `dependsOn` at all, so `lib` is only built because the generator reads it.
  */
class SourcegenProjectInputsIT extends IntegrationTestHarness {

  private val Yaml = """projects:
                       |  a:
                       |    extends: common
                       |    sourcegen:
                       |      project: scripts
                       |      main: testscripts.ListLib
                       |      inputs: lib
                       |  lib:
                       |    extends: common
                       |  scripts:
                       |    extends: common
                       |    dependencies: build.bleep::bleep-core:${BLEEP_VERSION}
                       |    platform:
                       |      jvmRuntimeOptions: -Xmx256m -Xms32m
                       |templates:
                       |  common:
                       |    platform:
                       |      name: jvm
                       |    scala:
                       |      version: 3.9.0
                       |""".stripMargin

  /** Lists the class files `lib` compiled to, by name, into a generated source. Reads `lib`'s output by path, the way such a generator does. */
  private val ListLib = """package testscripts
                          |
                          |import bleep.*
                          |import java.nio.file.Files
                          |import scala.jdk.CollectionConverters.*
                          |
                          |object ListLib extends BleepCodegenScript("ListLib") {
                          |  def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit = {
                          |    val classes = started.projectPaths(model.CrossProjectName(model.ProjectName("lib"), None)).classes
                          |    val names = Files.list(classes.resolve("lib")).iterator().asScala.map(_.getFileName.toString).toList.sorted
                          |    targets.foreach { target =>
                          |      val file = target.sources / "listed" / "Listed.scala"
                          |      Files.createDirectories(file.getParent)
                          |      Files.writeString(file, "package listed\nobject Listed { val names = \"" + names.mkString(",") + "\" }\n")
                          |    }
                          |  }
                          |}
                          |""".stripMargin

  private val a = model.CrossProjectName(model.ProjectName("a"), None)

  private def listed(ws: Workspace): String = {
    val files = Files.walk(ws.root.resolve(".bleep")).iterator().asScala.filter(_.getFileName.toString == "Listed.scala").toList
    files match {
      case List(one: Path) => Files.readString(one)
      case other           => fail(s"expected one generated Listed.scala, found ${other.mkString(", ")}")
    }
  }

  integrationTest("an input is built before the generator runs, and changing it runs the generator again") { ws =>
    ws.yaml(Yaml)
    ws.file("scripts/src/scala/testscripts/ListLib.scala", ListLib)
    ws.file("lib/src/scala/lib/Lib.scala", "package lib\nobject Lib\n")
    ws.file("a/src/scala/a/A.scala", "package a\nobject A { val n = listed.Listed.names }\n")

    val (_, commands, _) = ws.start()
    commands.compile(List(a))
    assert(listed(ws).contains("Lib.class"), listed(ws))
    assert(!listed(ws).contains("Lib2.class"), listed(ws))

    ws.file("lib/src/scala/lib/Lib2.scala", "package lib\nobject Lib2\n")
    commands.compile(List(a))
    assert(listed(ws).contains("Lib2.class"), listed(ws))
  }
}
