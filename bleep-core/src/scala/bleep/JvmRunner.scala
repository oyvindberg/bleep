package bleep

import bleep.internal.jvmRunCommand
import ryddig.Logger

import java.io.{ByteArrayOutputStream, PrintStream}
import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.Path

/** How `commands.run` (and similar) actually runs a JVM main class.
  *
  * Default is [[JvmRunner.Forked]] — assemble `java <jvmOptions> -cp <cp> <main> <args>` and exec via [[cli]]. Integration tests instead use
  * [[JvmRunner.InProcess]] which loads the main class in a [[URLClassLoader]] and invokes `main` reflectively in the test JVM itself. Forking a JVM per IT
  * meant the JVM-ergonomics 25%-of-RAM heap default × per-fork × parallelism × CI runner count routinely tipped 16 GB GHA runners into kernel OOM-kill.
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

  /** Test-only: load the main class in a [[URLClassLoader]] and invoke `main` in the current JVM. Avoids the per-IT JVM fork — all integration tests share the
    * test runner JVM, eliminating the OOM-kill surface that forking introduced on CI.
    *
    * `-D<key>=<value>` flags from `jvmOptions` are applied as system properties (snapshot/restore around the call). Other JVM flags (`-Xmx`, `-Xms`, `-X*`,
    * `-XX:*`) are silently ignored — they have no in-process equivalent. `cwd` and `env` are likewise ignored: there is no way to change a JVM's working
    * directory or environment from within. ITs that depend on those, or that assert on a child process's exit code or jvm-options, must keep using [[Forked]].
    *
    * `System.out` / `System.err` are redirected to feed the supplied logger (`info` / `warn` respectively) for the duration of the call, then restored. Tests
    * in one [[IntegrationTestHarness]] subclass run sequentially under ScalaTest, so single save-and-restore is sufficient — concurrent in-process invocations
    * from different test classes get their own test-runner JVM (and thus their own `System.out`).
    *
    * The classloader's parent is the platform classloader, so each invocation gets its own copy of every app-level class from the supplied classpath (Scala
    * stdlib, user-deps, etc.) and the test JVM's own classes do not leak in. After main returns, the classloader is closed; thread leaks from main itself are
    * not chased — they accumulate across ITs in the same test runner JVM. Callers that need real isolation must use [[Forked]].
    */
  case object InProcess extends JvmRunner {

    /** Serializes concurrent InProcess invocations within one JVM. `System.out`, `System.err`, and `System.setProperty` are JVM-global; the snapshot/restore
      * pattern below is correct only under exclusive access. Today there is no concurrent caller (one forked test-runner JVM runs one suite at a time, and
      * `AnyFunSuite` is sequential), but cheap insurance against a future change.
      */
    private val lock = new Object

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
    ): Either[BleepException, Unit] = lock.synchronized {
      val sysPropOverrides: List[(String, String)] = jvmOptions.collect {
        case s if s.startsWith("-D") =>
          val rest = s.substring(2)
          rest.indexOf('=') match {
            case -1 => rest -> ""
            case i  => rest.substring(0, i) -> rest.substring(i + 1)
          }
      }
      val ignored: List[String] = jvmOptions.filterNot(opt => opt.startsWith("-D") || opt.startsWith("--add-opens"))
      if (ignored.nonEmpty)
        logger.debug(s"InProcess JvmRunner ignoring jvmOptions: ${ignored.mkString(" ")}")

      val urls = classpath.map(_.toUri.toURL).toArray
      val cl = new URLClassLoader(urls, ClassLoader.getPlatformClassLoader)
      val savedOut = System.out
      val savedErr = System.err
      val savedCtx = Thread.currentThread.getContextClassLoader
      val savedProps: List[(String, Option[String])] =
        sysPropOverrides.map { case (k, _) => k -> Option(System.getProperty(k)) }

      val ctxLogger = logger.withPath(s"[in-process: $mainClass]")
      val capturedOut = new LineCapturingPrintStream(line => ctxLogger.info(line))
      val capturedErr = new LineCapturingPrintStream(line => ctxLogger.warn(line))
      System.setOut(capturedOut)
      System.setErr(capturedErr)
      sysPropOverrides.foreach { case (k, v) => System.setProperty(k, v) }
      Thread.currentThread.setContextClassLoader(cl)

      val result: Either[BleepException, Unit] =
        try {
          val mainKlass = Class.forName(mainClass, /*initialize*/ true, cl)
          val mainMethod = mainKlass.getMethod("main", classOf[Array[String]])
          try {
            mainMethod.invoke(null, args.toArray)
            Right(())
          } catch {
            case e: java.lang.reflect.InvocationTargetException =>
              val cause = Option(e.getCause).getOrElse(e)
              Left(new BleepException.Cause(cause, s"In-process run of $mainClass failed"))
          }
        } catch {
          case e: ClassNotFoundException => Left(new BleepException.Cause(e, s"Main class $mainClass not found on classpath"))
          case e: NoSuchMethodException  => Left(new BleepException.Cause(e, s"$mainClass has no main(Array[String]) method"))
        } finally {
          capturedOut.flush()
          capturedErr.flush()
          capturedOut.drainPartial()
          capturedErr.drainPartial()
          System.setOut(savedOut)
          System.setErr(savedErr)
          Thread.currentThread.setContextClassLoader(savedCtx)
          savedProps.foreach {
            case (k, Some(v)) => System.setProperty(k, v)
            case (k, None)    => System.clearProperty(k)
          }
          cl.close()
        }
      result
    }
  }

  /** PrintStream that buffers bytes until newline, then emits the assembled line as UTF-8. Used by [[InProcess]] to redirect `System.out` / `System.err` into
    * structured log events. Decodes once per line so multibyte UTF-8 chars survive.
    */
  private final class LineCapturingPrintStream(emitLine: String => Unit)
      extends PrintStream(new ByteArrayOutputStream(), /*autoFlush*/ true, StandardCharsets.UTF_8) {
    private val baos = new ByteArrayOutputStream()
    override def write(b: Int): Unit = synchronized {
      if (b == '\n') {
        emitLine(new String(baos.toByteArray, StandardCharsets.UTF_8))
        baos.reset()
      } else if (b != '\r') baos.write(b)
    }
    override def write(arr: Array[Byte], off: Int, len: Int): Unit = synchronized {
      var i = off
      val end = off + len
      while (i < end) { write(arr(i) & 0xff); i += 1 }
    }
    def drainPartial(): Unit = synchronized {
      if (baos.size > 0) {
        emitLine(new String(baos.toByteArray, StandardCharsets.UTF_8))
        baos.reset()
      }
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
