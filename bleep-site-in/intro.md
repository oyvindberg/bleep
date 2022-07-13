---
sidebar_position: 3
---

# Getting started

To install bleep, first follow instructions to install [coursier CLI](https://get-coursier.io/docs/cli-installation).
Then install bleep

```sh
$ cs install bleep
```


```scala mdoc
import bleep._

import mdoc.{DocusaurusPlugin, MdocPlugin}

import java.nio.file.Path
import scala.concurrent.ExecutionContext

object GenDocumentation {
  def main(args: Array[String]): Unit =
    bootstrap.forScript("GenDocs") { (started, commands) =>
      val projectName = model.CrossProjectName(model.ProjectName("scripts"), crossId = None)

      commands.compile(List(projectName))

      val mdoc = new MdocPlugin(started, projectName) {
        override def mdocIn: Path = started.buildPaths.buildDir / "bleep-site-in"
        override def mdocOut: Path = started.buildPaths.buildDir / "bleep-site" / "docs"
        override def mdocVariables = Map("VERSION" -> "1")
      }

      val docusaurus = new DocusaurusPlugin(
        started.buildPaths.buildDir / "bleep-site",
        mdoc,
        "bleep-site",
        Path.of("yarn"),
        started.logger,
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
```