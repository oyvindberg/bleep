package bleep
package scripts

import bleep.plugin.scalafix.ScalafixPlugin
import bleep.rewrites.semanticDb

object Scalafix extends BleepScript("Scalafix") {
  override val rewrites = List(new semanticDb)

  def run(started: Started, commands: Commands, args: List[String]): Unit = {
    val projects = started.globs.projectNameMap.get("jvm3").toList.flatten

    commands.compile(projects)

    new ScalafixPlugin(
      started,
      scalafixIvyDeps = List(
//        model.Dep.Scala("com.github.xuwei-k", "scalafix-rules", "0.3.0")
      )
    ).fix(
      projects = projects,
      args = args
    )
  }
}
