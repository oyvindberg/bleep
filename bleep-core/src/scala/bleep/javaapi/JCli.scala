package bleep.javaapi

import bleep.{cli, Started}

import java.nio.file.Path
import scala.jdk.CollectionConverters.*

/** Bridge for {@code bleepscript.Cli} — forks a process through {@code bleep.cli.apply}. */
object JCli {

  def runCli(
      jstarted: bleepscript.Started,
      action: String,
      command: java.util.List[String],
      cwd: Path,
      env: java.util.List[java.util.Map.Entry[String, String]]
  ): bleepscript.Cli.Result = {
    val started: Started = jstarted match {
      case js: JStarted => js.underlying
      case _            => throw new RuntimeException(s"Unknown Started impl: ${jstarted.getClass}")
    }

    val written = cli(
      action = action,
      cwd = cwd,
      cmd = command.asScala.toList,
      logger = started.logger,
      out = cli.Out.ViaLogger(started.logger),
      in = cli.In.No,
      env = env.asScala.iterator.map(e => e.getKey -> e.getValue).toList
    )

    new bleepscript.Cli.Result(
      written.stdout.toList.asJava,
      written.stderr.toList.asJava
    )
  }
}
