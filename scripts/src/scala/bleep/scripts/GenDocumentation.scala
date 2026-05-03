package bleep
package scripts

import java.io.File
import java.nio.file.Path

object GenDocumentation extends BleepScript("GenDocumentation") {
  override def run(started: Started, commands: Commands, args: List[String]): Unit = {
    // The three docs-snippet projects MUST compile cleanly — that's how we verify every
    // code sample on the site still builds. Run their compile before npm picks the
    // snippets up.
    commands.compile(
      List(
        model.CrossProjectName(model.ProjectName("docs-snippets-java"), crossId = None),
        model.CrossProjectName(model.ProjectName("docs-snippets-kotlin"), crossId = None),
        model.CrossProjectName(model.ProjectName("docs-snippets-scala"), crossId = None)
      )
    )

    val siteRoot = started.buildPaths.buildDir / "bleep-site"

    val nodeBinPath = started.pre.fetchNode("24.13.0").getParent
    started.logger.withContext("nodeBinPath", nodeBinPath).info("Using node")

    val env = sys.env.toList.collect {
      case x @ ("SSH_AUTH_SOCK", _) => x
      case ("PATH", value)          => "PATH" -> s"$nodeBinPath${File.pathSeparator}$value"
    }

    args.headOption match {
      case Some("dev") =>
        runNpm(siteRoot, env, started.logger, "install")
        // npm start runs extract-snippets.js then docusaurus start. inheritIO so
        // CTRL+C works and the user sees the dev server URL.
        runNpmInteractive(siteRoot, env, started.logger, "start")
      case Some("deploy") =>
        runNpm(siteRoot, env, started.logger, "install")
        runNpm(siteRoot, env, started.logger, "run", "build")
        // gh-pages deploy via docusaurus' built-in command. Caller must have ssh set up
        // and GIT_USER set in env.
        runNpm(siteRoot, env, started.logger, "run", "deploy")
        started.logger.info("Published to gh-pages")
      case Some(other) =>
        sys.error(s"Expected argument to be dev or deploy, not $other")
      case None =>
        runNpm(siteRoot, env, started.logger, "install")
        runNpm(siteRoot, env, started.logger, "run", "build")
        started.logger.info(s"Created documentation at ${siteRoot / "build"}")
    }
  }

  private def runNpm(cwd: Path, env: List[(String, String)], logger: ryddig.Logger, args: String*): Unit = {
    val cmd = "npm" :: args.toList
    logger.withContext("cmd", cmd.mkString(" ")).info(s"running in $cwd")
    val pb = new ProcessBuilder(cmd*)
    pb.directory(cwd.toFile)
    pb.redirectErrorStream(true)
    pb.environment().clear()
    env.foreach { case (k, v) => pb.environment().put(k, v) }
    val p = pb.start()
    val out = scala.io.Source.fromInputStream(p.getInputStream)
    out.getLines().foreach(line => logger.info(line))
    out.close()
    val exit = p.waitFor()
    if (exit != 0) sys.error(s"npm ${args.mkString(" ")} failed with exit code $exit")
  }

  private def runNpmInteractive(cwd: Path, env: List[(String, String)], logger: ryddig.Logger, args: String*): Unit = {
    val cmd = "npm" :: args.toList
    logger.withContext("cmd", cmd.mkString(" ")).info(s"running in $cwd")
    val pb = new ProcessBuilder(cmd*)
    pb.directory(cwd.toFile)
    pb.inheritIO()
    pb.environment().clear()
    env.foreach { case (k, v) => pb.environment().put(k, v) }
    val p = pb.start()
    val exit = p.waitFor()
    if (exit != 0) sys.error(s"npm ${args.mkString(" ")} failed with exit code $exit")
  }
}
