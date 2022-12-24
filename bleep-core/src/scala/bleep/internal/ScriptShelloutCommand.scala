package bleep
package internal

object ScriptShelloutCommand {
  def getForShellScript(scriptDef: model.ScriptDef.Shell): List[String] = {
    val bareCommand: String = scriptDef.`override-os`.flatMap(_.get(OsArch.current.os)).orElse(scriptDef.command).getOrElse {
      throw new BleepException.Text(s"no command found for os ${OsArch.current.os}")
    }
    OsArch.current.os match {
      case bleep.model.Os.Windows =>
        List("cmd.exe", "/C", bareCommand)
      case _ =>
        val quote = '"'.toString
        val quotedQuote = "\\\\"
        List("bash", "-c", bareCommand.replace(quote, quotedQuote))
    }
  }
}
