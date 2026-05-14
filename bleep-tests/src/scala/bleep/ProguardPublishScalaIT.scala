package bleep

/** Scala sibling of [[ProguardPublishJavaIT]]. Same publish flow (compile -> package -> ProGuard -> publishToLocalIvy), written as a Scala bleep script. */
class ProguardPublishScalaIT extends IntegrationTestHarness {

  integrationTest("proguard-publish-scala workspace compiles") { ws =>
    ws.yaml(
      snippet = "proguard-publish-scala/bleep.yaml",
      content = s"""projects:
                   |  mylib:
                   |    platform:
                   |      name: jvm
                   |    publish:
                   |      groupId: com.example
                   |    scala:
                   |      version: ${model.VersionScala.Scala3.scalaVersion}
                   |  scripts:
                   |    dependencies:
                   |      - build.bleep:bleepscript:$${BLEEP_VERSION}
                   |    platform:
                   |      name: jvm
                   |    scala:
                   |      version: ${model.VersionScala.Scala3.scalaVersion}
                   |scripts:
                   |  proguard-publish:
                   |    main: scripts.ProguardPublish
                   |    project: scripts
                   |""".stripMargin
    )

    ws.file(
      "mylib/src/scala/com/example/MyLib.scala",
      snippet = "proguard-publish-scala/MyLib.scala",
      content = """package com.example
                  |
                  |object MyLib:
                  |  def greet(name: String): String = s"Hello, $name!"
                  |""".stripMargin
    )

    ws.file(
      "scripts/src/scala/scripts/Proguard.scala",
      snippet = "proguard-publish-scala/Proguard.scala",
      content = """package scripts
                  |
                  |import bleepscript.{Cli, Coursier, Started}
                  |
                  |import java.io.File
                  |import java.nio.file.{Files, Path}
                  |import java.util.Comparator
                  |import scala.jdk.CollectionConverters.*
                  |import scala.util.Using
                  |
                  |/** A reusable ProGuard helper. Takes JAR bytes, returns shrunk JAR bytes.
                  |  * Reusable across projects: copy this file, depend on it via `sources:` in
                  |  * bleep.yaml, or publish it as a library.
                  |  *
                  |  * This is what a "plugin" looks like when you don't have a plugin API: a
                  |  * regular class with a regular method. No registration, no lifecycle, no
                  |  * autoplugin trigger — just code you can call.
                  |  *
                  |  * Instantiate via [[Proguard.apply]] — the factory fetches ProGuard once and
                  |  * caches the classpath. Reuse one instance across multiple shrink calls in
                  |  * the same script.
                  |  */
                  |class Proguard private (started: Started, proguardVersion: String, classpath: String):
                  |
                  |  /** Run ProGuard over the given JAR bytes; return the shrunk JAR bytes.
                  |    * Writes input/output JARs to a temp directory and forks a JVM through
                  |    * bleep's cli wrapper.
                  |    */
                  |  def shrink(inputJarBytes: Array[Byte]): Array[Byte] =
                  |    val workdir = Files.createTempDirectory("proguard")
                  |    try
                  |      val inputJar = workdir.resolve("input.jar")
                  |      val outputJar = workdir.resolve("output.jar")
                  |      val configFile = workdir.resolve("config.pro")
                  |      Files.write(inputJar, inputJarBytes)
                  |      Files.writeString(configFile, Proguard.config(inputJar, outputJar))
                  |
                  |      started.logger.info(s"running ProGuard $proguardVersion")
                  |      Cli.command("proguard")
                  |        .args(
                  |          started.jvmCommand.toString,
                  |          "-cp",
                  |          classpath,
                  |          "proguard.ProGuard",
                  |          s"@${configFile.toAbsolutePath}"
                  |        )
                  |        .cwd(workdir)
                  |        .run(started)
                  |
                  |      Files.readAllBytes(outputJar)
                  |    finally
                  |      Using.resource(Files.walk(workdir)): stream =>
                  |        stream.sorted(Comparator.reverseOrder).forEach(Files.delete)
                  |
                  |object Proguard:
                  |  val DefaultVersion = "7.7.0"
                  |
                  |  /** Resolve and cache the ProGuard classpath. */
                  |  def apply(started: Started, proguardVersion: String = DefaultVersion): Proguard =
                  |    val jars = Coursier.fetchClasspath(started, s"com.guardsquare:proguard-base:$proguardVersion")
                  |    val classpath = jars.asScala.map(_.toString).mkString(File.pathSeparator)
                  |    new Proguard(started, proguardVersion, classpath)
                  |
                  |  /** Minimal ProGuard config: shrink only (no obfuscation or optimization),
                  |    * keep all public members so the published artifact stays byte-compatible.
                  |    * Customize for real use.
                  |    */
                  |  private def config(inputJar: Path, outputJar: Path): String =
                  |    List(
                  |      s"-injars ${inputJar.toAbsolutePath}",
                  |      s"-outjars ${outputJar.toAbsolutePath}",
                  |      "-dontoptimize",
                  |      "-dontobfuscate",
                  |      "-dontwarn",
                  |      "-keep class ** { *; }"
                  |    ).mkString("\n")
                  |""".stripMargin
    )

    ws.file(
      "scripts/src/scala/scripts/ProguardPublish.scala",
      snippet = "proguard-publish-scala/ProguardPublish.scala",
      content = """package scripts
                  |
                  |import bleepscript.{
                  |  BleepScript,
                  |  BleepscriptServices,
                  |  Commands,
                  |  CrossProjectName,
                  |  Packaging,
                  |  PublishLayout,
                  |  Started
                  |}
                  |
                  |import java.util.Optional
                  |import scala.jdk.CollectionConverters.*
                  |
                  |/** Compose: compile, package, [[Proguard.shrink]] the main JAR, publish.
                  |  *
                  |  * The "plugin" lives next door in [[Proguard]]. This script is just glue: arg
                  |  * parsing + four bleepscript API calls.
                  |  *
                  |  * Usage: `bleep proguard-publish <projectName> <version> <groupId>`.
                  |  */
                  |class ProguardPublish extends BleepScript("proguard-publish"):
                  |
                  |  override def run(started: Started, commands: Commands, args: java.util.List[String]): Unit =
                  |    val argList = args.asScala.toList
                  |    require(argList.length >= 3, "Usage: bleep proguard-publish <projectName> <version> <groupId>")
                  |    val List(projectName, version, groupId, _*) = argList
                  |
                  |    val project = new CrossProjectName(projectName, Optional.empty())
                  |    val log = started.logger.withContext("project", projectName)
                  |
                  |    commands.compile(java.util.List.of(project))
                  |
                  |    log.info("packaging")
                  |    val library = Packaging.packageProject(
                  |      started,
                  |      project,
                  |      groupId,
                  |      version,
                  |      PublishLayout.Ivy.INSTANCE,
                  |      BleepscriptServices.Holder.INSTANCE.defaultManifestCreator()
                  |    )
                  |
                  |    val originalJar = library.jarFile
                  |    log.info(s"main JAR: ${library.jarFilePath.asString} (${originalJar.length} bytes)")
                  |
                  |    val shrunk = Proguard(started).shrink(originalJar)
                  |    log.info(s"ProGuard output: ${shrunk.length} bytes (saved ${originalJar.length - shrunk.length})")
                  |
                  |    Packaging.publishToLocalIvy(library.withJarFile(shrunk))
                  |    log.info(s"Published $groupId:$projectName:$version")
                  |""".stripMargin
    )

    ws.compileAll()
    succeed
  }
}
