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
}
