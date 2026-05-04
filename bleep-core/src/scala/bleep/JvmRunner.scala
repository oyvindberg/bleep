package bleep

import bleep.internal.jvmRunCommand
import ryddig.Logger

import java.nio.file.Path

/** How `commands.run` (and similar) actually runs a JVM main class.
  *
  * Default is [[JvmRunner.Forked]] — assemble `java <jvmOptions> -cp <cp> <main> <args>` and exec via [[cli]]. Integration tests swap in
  * [[JvmRunner.CappedFork]] which is the same fork but with a `-Xmx` cap prepended when the user hasn't set one themselves. Otherwise the JVM ergonomics
  * default of `-XX:MaxRAMPercentage=25.0` commits ~4 GB heap on a 16 GB GHA runner per forked Main, and parallel ITs push the runner into OS OOM-kill.
  */
trait JvmRunner {
  def run(
      cwd: Path,
      resolvedJvm: ResolvedJvm,
      classpath: List[Path],
      jvmOptions: List[String],
      mainClass: String,
      args: List[String],
      env: List[(String, String)],
      logger: Logger,
      raw: Boolean
  ): Either[BleepException, Unit]
}

object JvmRunner {

  /** Production default: fork via [[cli]] with the user-provided jvmOptions verbatim. */
  case object Forked extends JvmRunner {
    override def run(
        cwd: Path,
        resolvedJvm: ResolvedJvm,
        classpath: List[Path],
        jvmOptions: List[String],
        mainClass: String,
        args: List[String],
        env: List[(String, String)],
        logger: Logger,
        raw: Boolean
    ): Either[BleepException, Unit] =
      forkWith(jvmOptions, cwd, resolvedJvm, classpath, mainClass, args, env, logger, raw)
  }

  /** Fork like [[Forked]], but if the user hasn't constrained the heap (no `-Xmx` or `-XX:MaxRAMPercentage` in their jvmOptions), prepend `-Xmx<maxHeap>`. Used
    * by integration tests so each forked Main commits a small heap rather than the JVM ergonomics default of 25% of system RAM (~4 GB on a 16 GB host).
    */
  case class CappedFork(maxHeap: String) extends JvmRunner {
    override def run(
        cwd: Path,
        resolvedJvm: ResolvedJvm,
        classpath: List[Path],
        jvmOptions: List[String],
        mainClass: String,
        args: List[String],
        env: List[(String, String)],
        logger: Logger,
        raw: Boolean
    ): Either[BleepException, Unit] = {
      val capped =
        if (jvmOptions.exists(opt => opt.startsWith("-Xmx") || opt.startsWith("-XX:MaxRAMPercentage"))) jvmOptions
        else s"-Xmx$maxHeap" :: jvmOptions
      forkWith(capped, cwd, resolvedJvm, classpath, mainClass, args, env, logger, raw)
    }
  }

  private def forkWith(
      jvmOptions: List[String],
      cwd: Path,
      resolvedJvm: ResolvedJvm,
      classpath: List[Path],
      mainClass: String,
      args: List[String],
      env: List[(String, String)],
      logger: Logger,
      raw: Boolean
  ): Either[BleepException, Unit] = {
    val outMode = if (raw) cli.Out.Raw else cli.Out.ViaLogger(logger)
    val inMode = if (raw) cli.In.Attach else cli.In.No
    val command = jvmRunCommand.cmd(resolvedJvm, jvmOptions, classpath, mainClass, args)
    cli("run", cwd, command, logger = logger, out = outMode, in = inMode, env = env).discard()
    Right(())
  }
}
