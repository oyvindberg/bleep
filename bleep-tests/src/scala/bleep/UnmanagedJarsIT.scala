package bleep

import java.nio.file.{Files, Path}
import java.util.jar.JarOutputStream

class UnmanagedJarsIT extends IntegrationTestHarness {
  private def createEmptyJar(jarPath: Path): Unit = {
    Files.createDirectories(jarPath.getParent)
    val out = new JarOutputStream(Files.newOutputStream(jarPath))
    out.close()
  }

  integrationTest("unmanaged jars on classpath") { ws =>
    createEmptyJar(ws.root.resolve("lib/foo.jar"))
    createEmptyJar(ws.root.resolve("lib/bar.jar"))

    ws.yaml(
      """projects:
        |  myapp:
        |    jars:
        |      - lib/foo.jar
        |      - lib/bar.jar
        |    platform:
        |      name: jvm
        |    scala:
        |      version: 3.3.3
        |""".stripMargin
    )
    ws.file(
      "myapp/src/scala/Main.scala",
      """package test
        |object Main {
        |  def main(args: Array[String]): Unit = println("ok")
        |}
        |""".stripMargin
    )

    val (started, _, _) = ws.start()
    val projectName = model.CrossProjectName(model.ProjectName("myapp"), None)

    val exploded = started.build.explodedProjects(projectName)
    assert(exploded.jars.values.size === 2, s"Expected 2 jars, got: ${exploded.jars.values}")

    val resolved = started.resolvedProjects(projectName).forceGet("test")
    val fooJar = resolved.classpath.find(_.toString.contains("foo.jar"))
    val barJar = resolved.classpath.find(_.toString.contains("bar.jar"))
    assert(fooJar.isDefined, s"Expected foo.jar on classpath, got: ${resolved.classpath.mkString(", ")}")
    assert(barJar.isDefined, s"Expected bar.jar on classpath, got: ${resolved.classpath.mkString(", ")}")
    assert(Files.exists(fooJar.get), s"Jar file does not exist: ${fooJar.get}")
    assert(Files.exists(barJar.get), s"Jar file does not exist: ${barJar.get}")
    succeed
  }
}
