package bleep
package commands

/** `bleep script`: every script in the build, sorted by name, with what it does. A build with dozens of scripts is unusable from `bleep --help` alone. */
case class ListScripts(mode: OutputMode) extends BleepBuildCommand {
  override def run(started: Started): Either[BleepException, Unit] = {
    val scripts = started.build.scripts.toList.sortBy { case (scriptName, _) => scriptName.value }
    mode match {
      case OutputMode.Text =>
        if (scripts.isEmpty) started.logger.info("This build has no scripts")
        else ListScripts.render(scripts).foreach(started.logger.info(_))
      case OutputMode.Json =>
        CommandResult.print(CommandResult.success(ExtractInfo.ScriptsOutput(ExtractInfo.scriptInfos(started.build))))
      case OutputMode.Raw =>
        scripts.foreach { case (scriptName, _) => println(scriptName.value) }
    }
    Right(())
  }
}

object ListScripts {

  /** One line per script: the name, padded so the descriptions line up. A script without a description shows what it runs instead. */
  def render(scripts: List[(model.ScriptName, model.JsonList[model.ScriptDef])]): List[String] = {
    val width = scripts.map { case (scriptName, _) => scriptName.value.length }.max
    scripts.map { case (scriptName, scriptDefs) =>
      val text = model.ScriptDef.description(scriptDefs.values).getOrElse(runs(scriptDefs.values))
      s"${scriptName.value.padTo(width, ' ')}  $text"
    }
  }

  private def runs(scriptDefs: List[model.ScriptDef]): String =
    scriptDefs.map { case x: model.ScriptDef.Main => s"${x.project.value}/${x.main}" }.mkString("(", ", ", ")")
}
