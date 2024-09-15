package bleep
package scripts

import bleep.plugin.mdoc.{DocusaurusPlugin, MdocPlugin}

import java.io.File
import java.nio.file.Path

object GenDocumentation extends BleepScript("GenDocumentation") {
  override def run(started: Started, commands: Commands, args: List[String]): Unit = {
    val scriptsProject = model.CrossProjectName(model.ProjectName("scripts"), crossId = None)

    commands.compile(List(scriptsProject))

    val mdoc = new MdocPlugin(started, scriptsProject) {
      override def mdocIn: Path = started.buildPaths.buildDir / "bleep-site-in"
      override def mdocOut: Path = started.buildPaths.buildDir / "bleep-site" / "docs"
    }

    val nodeBinPath = started.pre.fetchNode(constants.Node).getParent

    started.logger.withContext("nodeBinPath", nodeBinPath).info("Using node")

    val env = sys.env.collect {
      case x @ ("SSH_AUTH_SOCK", _) => x
      case ("PATH", value)          => "PATH" -> s"$nodeBinPath${File.pathSeparator}$value"
    }.toList

    val docusaurus = new DocusaurusPlugin(
      website = started.buildPaths.buildDir / "bleep-site",
      mdoc = mdoc,
      docusaurusProjectName = "bleep-site",
      env = env,
      logger = started.logger,
      isDocusaurus2 = true
    )

    args.headOption match {
      case Some("dev") =>
        docusaurus.dev(started.executionContext)
      case Some("deploy") =>
        docusaurus.docusaurusPublishGhpages(mdocArgs = Nil)
      case Some(other) =>
        sys.error(s"Expected argument to be dev or deploy, not $other")
      case None =>
        val path = docusaurus.doc(mdocArgs = args)
        started.logger.info(s"Created documentation at $path")
    }
  }
}
