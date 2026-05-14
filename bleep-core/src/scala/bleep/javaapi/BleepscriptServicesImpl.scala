package bleep.javaapi

import bleep.{bootstrap, model, BleepCodegenScript, PreBootstrapOpts, RelPath}

import scala.jdk.CollectionConverters.*

final class BleepscriptServicesImpl extends bleepscript.BleepscriptServices {

  override def forScript(scriptName: String, args: Array[String], script: bleepscript.BleepScript): Unit = {
    val (preOpts, restArgs) = PreBootstrapOpts.parse(args.toList)
    bootstrap.forScript(scriptName, preOpts.toLoggingOpts) { (started, _) =>
      val jStarted = new JStarted(started)
      val jCommands = new JCommands(started)
      script.run(jStarted, jCommands, restArgs.asJava)
    }
  }

  override def forCodegen(
      scriptName: String,
      thisClassName: String,
      args: Array[String],
      script: bleepscript.BleepCodegenScript
  ): Unit = {
    // Drive the Scala BleepCodegenScript machinery via an anonymous subclass so we get
    // temp-dir sync + stamp files for free.
    val adapter = new BleepCodegenScript(scriptName) {
      override val ThisClassName: String = thisClassName
      override def run(
          started: bleep.Started,
          commands: bleep.Commands,
          targets: List[Target],
          args: List[String]
      ): Unit = {
        val jStarted = new JStarted(started)
        val jCommands = new JCommands(started)
        val jTargets = targets.map(t =>
          new bleepscript.CodegenTarget(
            JModel.crossProjectName(t.project),
            t.sources,
            t.resources
          )
        )
        script.run(jStarted, jCommands, jTargets.asJava, args.asJava)
      }
    }
    adapter.main(args)
  }

  override def parseDep(coordinates: String): bleepscript.Dep =
    model.Dep.parse(coordinates) match {
      case Right(d)  => JModel.dep(d)
      case Left(err) => throw new IllegalArgumentException(err)
    }

  override def parseRelPath(path: String): bleepscript.RelPath =
    RelPath(path) match {
      case Right(r)  => JModel.relPath(r)
      case Left(err) => throw new IllegalArgumentException(err)
    }

  override def defaultManifestCreator(): bleepscript.ManifestCreator =
    JModel.defaultManifestCreator()

  override def packageProject(
      started: bleepscript.Started,
      project: bleepscript.CrossProjectName,
      fallbackGroupId: String,
      version: String,
      layout: bleepscript.PublishLayout,
      manifestCreator: bleepscript.ManifestCreator
  ): bleepscript.PackagedLibrary =
    JPackaging.packageProject(started, project, fallbackGroupId, version, layout, manifestCreator)

  override def packageProjects(
      started: bleepscript.Started,
      projects: java.util.List[bleepscript.CrossProjectName],
      fallbackGroupId: String,
      version: String,
      layout: bleepscript.PublishLayout,
      manifestCreator: bleepscript.ManifestCreator
  ): java.util.Map[bleepscript.CrossProjectName, bleepscript.PackagedLibrary] =
    JPackaging.packageProjects(started, projects, fallbackGroupId, version, layout, manifestCreator)

  override def createJar(
      jarType: bleepscript.JarType,
      manifestCreator: bleepscript.ManifestCreator,
      fromFolders: java.util.List[java.nio.file.Path],
      projectName: java.util.Optional[bleepscript.CrossProjectName],
      mainClass: java.util.Optional[String]
  ): Array[Byte] =
    JPackaging.createJar(jarType, manifestCreator, fromFolders, projectName, mainClass)

  override def publishToLocalIvy(library: bleepscript.PackagedLibrary): Unit =
    JPackaging.publishToLocalIvy(library)

  override def publishToLocalMaven(library: bleepscript.PackagedLibrary): Unit =
    JPackaging.publishToLocalMaven(library)

  override def publishToFolder(library: bleepscript.PackagedLibrary, folder: java.nio.file.Path): Unit =
    JPackaging.publishToFolder(library, folder)

  override def publishToResolver(
      started: bleepscript.Started,
      library: bleepscript.PackagedLibrary,
      resolverName: String
  ): Unit =
    JPackaging.publishToResolver(started, library, resolverName)

  override def signArtifacts(
      started: bleepscript.Started,
      library: bleepscript.PackagedLibrary
  ): bleepscript.PackagedLibrary =
    JPackaging.signArtifacts(started, library)

  override def fetchClasspath(
      started: bleepscript.Started,
      coordinates: String
  ): java.util.List[java.nio.file.Path] =
    JCoursier.fetchClasspath(started, coordinates)

  override def runCli(
      started: bleepscript.Started,
      action: String,
      command: java.util.List[String],
      cwd: java.nio.file.Path,
      env: java.util.List[java.util.Map.Entry[String, String]]
  ): bleepscript.Cli.Result =
    JCli.runCli(started, action, command, cwd, env)
}
