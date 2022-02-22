package bleep
package commands

import ch.epfl.scala.bsp4j

import scala.build.bloop.BloopServer

case class Script(started: Started, name: model.ScriptName, scriptDefs: JsonList[model.ScriptDef], args: List[String]) extends BleepCommandRemote {
  override def runWithServer(bloop: BloopServer): Unit = {
    val targets = buildTargets(started.buildPaths, scriptDefs.values.map(_.project).distinct)
    bloop.server.buildTargetCompile(new bsp4j.CompileParams(targets)).get().getStatusCode match {
      case bsp4j.StatusCode.OK =>
        scriptDefs.values.foreach { scriptDef =>
          val bloopFile = started.bloopFiles(scriptDef.project).forceGet
          val fullClassPath = fixedClasspath(bloopFile.project)
          cli(s"java -cp ${fullClassPath.mkString(":")} ${scriptDef.main} ${args.mkString(" ")}")(started.buildPaths.buildDir)
        }
      case _ => started.logger.error(s"Not running script ${name.value} since compilation failed")
    }
  }
}
