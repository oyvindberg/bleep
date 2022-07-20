package bleep
package scripts

import mdoc.{DocusaurusPlugin, MdocPlugin}

import java.nio.file.Path
import scala.concurrent.ExecutionContext

object GenDocumentation {
  def main(args: Array[String]): Unit =
    bootstrap.forScript("GenDocs") { (started, commands) =>
      val projectName = model.CrossProjectName(model.ProjectName("scripts"), crossId = None)

      commands.compile(List(projectName))

      val bleepYamlContents =
        started.prebootstrapped.existingBuild.str.forceGet.toOption
          // avoid illegal references to capture group in regex replacement
          .map(str => str.replace("$", "\\$"))

      val mdoc = new MdocPlugin(started, projectName) {
        override def mdocIn: Path = started.buildPaths.buildDir / "bleep-site-in"
        override def mdocOut: Path = started.buildPaths.buildDir / "bleep-site" / "docs"
        override def mdocVariables =
          Map(
            "VERSION" -> Some("1"),
            "BLEEPYAML" -> bleepYamlContents
          ).collect { case (k, Some(v)) => (k, v) }
      }

      val docusaurus = new DocusaurusPlugin(
        website = started.buildPaths.buildDir / "bleep-site",
        mdoc = mdoc,
        docusaurusProjectName = "bleep-site",
        yarn = Path.of("yarn"),
        logger = started.logger,
        isDocusaurus2 = true
      )

      args.headOption match {
        case Some("dev") =>
          docusaurus.dev(ExecutionContext.global)
        case Some("deploy") =>
          docusaurus.docusaurusPublishGhpages(mdocArgs = Nil)
        case Some(other) =>
          sys.error(s"Expected argument to be dev or deploy, not $other")
        case None =>
          val path = docusaurus.doc(mdocArgs = args.toList)
          started.logger.info(s"Created documentation at $path")
      }
    }
}
