package bleep.testing

import cats.effect._
import cats.effect.std.{Queue, Semaphore}
import cats.syntax.all._
import fs2.Stream

import java.io._
import java.nio.file.Path
import java.security.MessageDigest
import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._
import scala.util.control.NonFatal

/** A pool of reusable JVM processes for running tests.
  *
  * JVMs are expensive to start, so we pool them by classpath hash. When a test needs a JVM with a particular classpath, we either return an existing idle JVM
  * or spawn a new one.
  *
  * Key features:
  *   - JVMs keyed by classpath + options hash for reuse
  *   - Bounded concurrency via semaphore
  *   - Explicit shutdown (no shutdown hooks)
  *   - Health checks before reuse
  */
trait JvmPool {

  /** Acquire a JVM suitable for the given classpath and options.
    *
    * Returns a Resource that will release the JVM back to the pool when done.
    */
  def acquire(
      classpath: List[Path],
      jvmOptions: List[String],
      runnerClass: String,
      environment: Map[String, String],
      workingDirectory: Option[Path]
  ): Resource[IO, TestJvm]

  /** Shutdown all JVMs in the pool.
    *
    * This MUST be called when done with the pool. Use guarantee to ensure it runs.
    */
  def shutdown: IO[Unit]

  /** Number of JVMs currently in the pool */
  def size: IO[Int]
}

/** A handle to a forked JVM running the test runner */
trait TestJvm {

  /** Process ID of this JVM */
  def pid: Long

  /** Run a test suite and stream back responses */
  def runSuite(
      className: String,
      framework: String,
      args: List[String]
  ): Stream[IO, TestProtocol.TestResponse]

  /** Get a thread dump from the JVM */
  def getThreadDump: IO[Option[TestProtocol.TestResponse.ThreadDump]]

  /** Read any available stderr lines (non-blocking) */
  def drainStderr: IO[List[String]]

  /** Check if the JVM process is still alive */
  def isAlive: IO[Boolean]

  /** Kill the JVM process immediately */
  def kill: IO[Unit]
}

object JvmPool {

  /** Create a new JVM pool with the given maximum concurrency.
    *
    * IMPORTANT: The returned pool has an explicit shutdown method that MUST be called when done. Use guarantee to ensure cleanup: {{{
    * JvmPool.create(maxConcurrency, jvmCommand, workingDirectory).use { pool => // pool.shutdown is called automatically when this scope ends runTests(pool) }
    * }}}
    *
    * No shutdown hooks are used - caller is responsible for ensuring shutdown is called.
    *
    * @param maxConcurrency
    *   Maximum number of JVMs to run concurrently
    * @param jvmCommand
    *   Path to the java binary (e.g., started.jvmCommand)
    */
  def create(
      maxConcurrency: Int,
      jvmCommand: Path,
      workingDirectory: Path
  ): Resource[IO, JvmPool] =
    Resource.make(
      for {
        semaphore <- Semaphore[IO](maxConcurrency.toLong)
        pool <- IO(new TrieMap[JvmKey, Queue[IO, ManagedJvm]]())
        allJvms <- Ref.of[IO, Set[ManagedJvm]](Set.empty)
      } yield new JvmPoolImpl(semaphore, pool, allJvms, new TrieMap[JvmKey, Int](), jvmCommand, workingDirectory)
    )(_.shutdown)

  /** Key for pooling JVMs */
  private case class JvmKey(classpathHash: String, optionsHash: String, envHash: String, cwdHash: String)

  private object JvmKey {
    def apply(classpath: List[Path], options: List[String], environment: Map[String, String], cwd: Option[Path]): JvmKey = {
      val cpHash = hashStrings(classpath.map(_.toString))
      val optHash = hashStrings(options)
      val envHash = hashStrings(environment.toList.sorted.map { case (k, v) => s"$k=$v" })
      val cwdHash = hashStrings(cwd.map(_.toString).toList)
      JvmKey(cpHash, optHash, envHash, cwdHash)
    }

    private def hashStrings(strings: List[String]): String = {
      val md = MessageDigest.getInstance("SHA-256")
      strings.foreach(s => md.update(s.getBytes("UTF-8")))
      md.digest().take(8).map("%02x".format(_)).mkString
    }
  }

  /** Internal managed JVM wrapper */
  private class ManagedJvm(
      val process: Process,
      val stdin: PrintWriter,
      val stdout: BufferedReader,
      val stderr: BufferedReader,
      val key: JvmKey
  ) {
    @volatile private var alive = true
    @volatile private var _protocolClean = true

    def isAlive: Boolean =
      alive && process.isAlive

    def protocolClean: Boolean = _protocolClean

    def markProtocolDirty(): Unit =
      _protocolClean = false

    def markDead(): Unit =
      alive = false

    def kill(): Unit = {
      alive = false
      try
        stdin.close()
      catch { case NonFatal(_) => }
      // Kill the entire process tree, not just the direct child.
      // If the test runner spawned sub-processes (e.g., for some test frameworks),
      // those would otherwise be orphaned and consume system resources.
      try
        process
          .descendants()
          .forEach(ph =>
            try ph.destroyForcibly()
            catch { case _: Exception => () }
          )
      catch { case NonFatal(_) => }
      process.destroyForcibly()
      try
        process.waitFor(5, java.util.concurrent.TimeUnit.SECONDS)
      catch { case NonFatal(_) => }
    }

    /** Read any available stderr output */
    def readStderr(): String = {
      val sb = new StringBuilder
      try
        while (stderr.ready()) {
          val line = stderr.readLine()
          if (line != null) {
            sb.append(line).append("\n")
          }
        }
      catch { case NonFatal(_) => }
      sb.toString()
    }
  }

  /** Max consecutive spawn failures per key before refusing to spawn. Prevents infinite retry when test runner jar is incompatible. */
  private val MaxSpawnFailures = 3

  private class JvmPoolImpl(
      semaphore: Semaphore[IO],
      pool: TrieMap[JvmKey, Queue[IO, ManagedJvm]],
      allJvms: Ref[IO, Set[ManagedJvm]],
      spawnFailures: TrieMap[JvmKey, Int],
      jvmCommand: Path,
      workingDirectory: Path
  ) extends JvmPool {

    override def acquire(
        classpath: List[Path],
        jvmOptions: List[String],
        runnerClass: String,
        environment: Map[String, String],
        workingDirectory: Option[Path]
    ): Resource[IO, TestJvm] = {
      val key = JvmKey(classpath, jvmOptions, environment, workingDirectory)

      Resource
        .make(
          for {
            _ <- semaphore.acquire
            jvm <- getOrCreate(key, classpath, jvmOptions, runnerClass, environment, workingDirectory)
          } yield (jvm, new TestJvmImpl(jvm): TestJvm)
        ) { case (jvm, _) =>
          // Return JVM to pool and release semaphore
          release(jvm) >> semaphore.release
        }
        .map(_._2)
    }

    private def getOrCreate(
        key: JvmKey,
        classpath: List[Path],
        jvmOptions: List[String],
        runnerClass: String,
        environment: Map[String, String],
        cwd: Option[Path]
    ): IO[ManagedJvm] =
      for {
        queue <- IO(
          pool.getOrElseUpdate(
            key, {
              // Create queue synchronously to avoid race
              import cats.effect.unsafe.implicits.global
              Queue.unbounded[IO, ManagedJvm].unsafeRunSync()
            }
          )
        )
        maybeJvm <- queue.tryTake
        jvm <- maybeJvm match {
          case Some(existing) if existing.isAlive =>
            IO.pure(existing)
          case Some(dead) =>
            // JVM died, remove from tracking and create new
            allJvms.update(_ - dead) >> spawnJvm(key, classpath, jvmOptions, runnerClass, environment, cwd)
          case None =>
            spawnJvm(key, classpath, jvmOptions, runnerClass, environment, cwd)
        }
      } yield jvm

    private def spawnJvm(
        key: JvmKey,
        classpath: List[Path],
        jvmOptions: List[String],
        runnerClass: String,
        environment: Map[String, String],
        cwdOverride: Option[Path]
    ): IO[ManagedJvm] = {
      val failures = spawnFailures.getOrElse(key, 0)
      if (failures >= MaxSpawnFailures) {
        return IO.raiseError(
          new IOException(
            s"Test JVM failed to start $failures consecutive times. This usually means the test runner jar " +
              s"is incompatible with the project's JVM. Check that bleep-test-runner is published for the correct Java version."
          )
        )
      }
      IO
        .blocking {
          val javaPath = jvmCommand
          val cpString = classpath.map(_.toString).mkString(File.pathSeparator)

          // On Windows, command-line length is limited to 32,767 characters.
          // When the classpath is too long, pass it via CLASSPATH environment variable instead.
          val useEnvClasspath = scala.util.Properties.isWin && cpString.length > 30000

          val cmd =
            if (useEnvClasspath)
              List(javaPath.toString) ++ jvmOptions ++ List(runnerClass)
            else
              List(javaPath.toString) ++ jvmOptions ++ List("-cp", cpString, runnerClass)

          val pb = new ProcessBuilder(cmd: _*)
          pb.directory(cwdOverride.getOrElse(workingDirectory).toFile)
          pb.redirectErrorStream(false)
          if (useEnvClasspath) {
            pb.environment().put("CLASSPATH", cpString)
          }
          environment.foreach { case (k, v) => pb.environment().put(k, v) }

          val process = pb.start()
          val stdin = new PrintWriter(new BufferedOutputStream(process.getOutputStream), true)
          val stdout = new BufferedReader(new InputStreamReader(process.getInputStream))
          val stderr = new BufferedReader(new InputStreamReader(process.getErrorStream))

          new ManagedJvm(process, stdin, stdout, stderr, key)
        }
        .flatTap(jvm => allJvms.update(_ + jvm))
        .flatTap(jvm =>
          waitForReady(jvm).onError { case _ =>
            IO(spawnFailures.updateWith(jvm.key) { case Some(n) => Some(n + 1); case None => Some(1) })
          }
        )
        .flatTap(jvm => IO(spawnFailures.remove(jvm.key))) // Reset on success
    }

    private def waitForReady(jvm: ManagedJvm): IO[Unit] =
      IO.interruptible {
        val line = jvm.stdout.readLine()
        if (line == null) {
          // Process terminated - capture stderr to show actual error
          Thread.sleep(100) // Give stderr a moment to be available
          val stderrOutput = jvm.readStderr()
          val errorMsg = if (stderrOutput.nonEmpty) {
            s"JVM process terminated before sending Ready. Stderr:\n$stderrOutput"
          } else {
            "JVM process terminated before sending Ready (no stderr output)"
          }
          throw new IOException(errorMsg)
        }
        TestProtocol.decodeResponse(line) match {
          case Right(TestProtocol.TestResponse.Ready) => ()
          case Right(other) =>
            throw new IOException(s"Expected Ready, got: $other")
          case Left(err) =>
            throw new IOException(s"Failed to decode response: $err, line: $line")
        }
      }.timeout(30.seconds)
        .onError { case _ => IO(jvm.kill()) }

    override def shutdown: IO[Unit] =
      // CRITICAL: Use uncancelable to ensure cleanup completes even during cancellation
      IO.uncancelable { _ =>
        for {
          jvms <- allJvms.get
          _ <- IO.blocking {
            jvms.foreach { jvm =>
              try {
                // Send shutdown command
                jvm.stdin.println(TestProtocol.encodeCommand(TestProtocol.TestCommand.Shutdown))
                jvm.stdin.flush()
              } catch { case NonFatal(_) => }
            }
          }
          // Give them a moment to shutdown gracefully
          _ <- IO.sleep(500.millis)
          _ <- IO.blocking {
            jvms.foreach(_.kill())
          }
          _ <- allJvms.set(Set.empty)
        } yield ()
      }

    override def size: IO[Int] =
      allJvms.get.map(_.size)

    /** Return a JVM to the pool for reuse */
    private def release(jvm: ManagedJvm): IO[Unit] =
      if (jvm.isAlive && jvm.protocolClean) {
        for {
          queue <- IO(
            pool.getOrElseUpdate(
              jvm.key, {
                import cats.effect.unsafe.implicits.global
                Queue.unbounded[IO, ManagedJvm].unsafeRunSync()
              }
            )
          )
          _ <- queue.offer(jvm)
        } yield ()
      } else {
        // Dead or protocol-dirty JVM — kill and remove from tracking
        IO(jvm.kill()).attempt >> allJvms.update(_ - jvm)
      }

    private class TestJvmImpl(jvm: ManagedJvm) extends TestJvm {

      override def pid: Long = jvm.process.pid()

      override def runSuite(
          className: String,
          framework: String,
          args: List[String]
      ): Stream[IO, TestProtocol.TestResponse] = {
        val command = TestProtocol.TestCommand.RunSuite(className, framework, args)

        Stream.eval(sendCommand(command)) >>
          readResponses.takeThrough {
            case _: TestProtocol.TestResponse.SuiteDone => false
            case _: TestProtocol.TestResponse.Error     => false
            case _                                      => true
          }
      }

      private def sendCommand(cmd: TestProtocol.TestCommand): IO[Unit] =
        IO.blocking {
          jvm.stdin.println(TestProtocol.encodeCommand(cmd))
          jvm.stdin.flush()
        }

      private def readResponses: Stream[IO, TestProtocol.TestResponse] =
        Stream.repeatEval {
          // Use interruptible so timeout/cancellation can interrupt the blocked readLine thread.
          // Periodically drain stderr to prevent buffer deadlock: if the child process fills
          // its stderr buffer while we're blocked on stdout, both processes deadlock.
          val readOne = IO.interruptible {
            val line = jvm.stdout.readLine()
            if (line == null) {
              jvm.markDead()
              None
            } else {
              TestProtocol.decodeResponse(line) match {
                case Right(response) => Some(response)
                case Left(err) =>
                  jvm.markProtocolDirty()
                  Some(
                    TestProtocol.TestResponse.Error(
                      s"Protocol error: ${err.getMessage}",
                      Some(s"Line: $line")
                    )
                  )
              }
            }
          }
          // Race readOne against a periodic stderr drain to prevent buffer deadlock.
          // Every 100ms while waiting for stdout, drain stderr.
          def drainLoop: IO[Unit] =
            IO.sleep(100.millis) >> IO.blocking(jvm.readStderr()).void >> drainLoop

          IO.race(readOne, drainLoop).map {
            case Left(result) => result
            case Right(_)     => None // drainLoop never completes, but required for types
          }
        }.unNoneTerminate

      override def getThreadDump: IO[Option[TestProtocol.TestResponse.ThreadDump]] =
        for {
          _ <- sendCommand(TestProtocol.TestCommand.GetThreadDump)
          response <- IO
            .interruptible {
              val line = jvm.stdout.readLine()
              if (line == null) {
                jvm.markDead()
                None
              } else {
                TestProtocol.decodeResponse(line) match {
                  case Right(td: TestProtocol.TestResponse.ThreadDump) => Some(td)
                  case _                                               => None
                }
              }
            }
            .timeout(5.seconds)
            .handleError(_ => None)
        } yield response

      override def drainStderr: IO[List[String]] =
        IO.blocking {
          val output = jvm.readStderr()
          if (output.isEmpty) Nil
          else output.split('\n').toList
        }

      override def isAlive: IO[Boolean] =
        IO(jvm.isAlive)

      override def kill: IO[Unit] =
        IO(jvm.kill())
    }
  }
}
