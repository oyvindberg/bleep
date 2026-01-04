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
  *   - Automatic cleanup on pool shutdown
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
      runnerClass: String
  ): Resource[IO, TestJvm]

  /** Shutdown all JVMs in the pool */
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

  /** Check if the JVM process is still alive */
  def isAlive: IO[Boolean]

  /** Kill the JVM process immediately */
  def kill: IO[Unit]
}

object JvmPool {

  /** Create a new JVM pool with the given maximum concurrency
    *
    * @param maxConcurrency
    *   Maximum number of JVMs to run concurrently
    * @param jvmCommand
    *   Path to the java binary (e.g., started.jvmCommand)
    */
  def create(
      maxConcurrency: Int,
      jvmCommand: Path
  ): Resource[IO, JvmPool] =
    Resource.make(
      for {
        semaphore <- Semaphore[IO](maxConcurrency.toLong)
        pool <- IO(new TrieMap[JvmKey, Queue[IO, ManagedJvm]]())
        allJvms <- Ref.of[IO, Set[ManagedJvm]](Set.empty)
      } yield new JvmPoolImpl(semaphore, pool, allJvms, jvmCommand)
    )(_.shutdown)

  /** Key for pooling JVMs */
  private case class JvmKey(classpathHash: String, optionsHash: String)

  private object JvmKey {
    def apply(classpath: List[Path], options: List[String]): JvmKey = {
      val cpHash = hashStrings(classpath.map(_.toString))
      val optHash = hashStrings(options)
      JvmKey(cpHash, optHash)
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

    def isAlive: Boolean =
      alive && process.isAlive

    def markDead(): Unit =
      alive = false

    def kill(): Unit = {
      alive = false
      try
        stdin.close()
      catch { case NonFatal(_) => }
      process.destroyForcibly()
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

  private class JvmPoolImpl(
      semaphore: Semaphore[IO],
      pool: TrieMap[JvmKey, Queue[IO, ManagedJvm]],
      allJvms: Ref[IO, Set[ManagedJvm]],
      jvmCommand: Path
  ) extends JvmPool {

    override def acquire(
        classpath: List[Path],
        jvmOptions: List[String],
        runnerClass: String
    ): Resource[IO, TestJvm] = {
      val key = JvmKey(classpath, jvmOptions)

      Resource
        .make(
          for {
            _ <- semaphore.acquire
            jvm <- getOrCreate(key, classpath, jvmOptions, runnerClass)
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
        runnerClass: String
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
            allJvms.update(_ - dead) >> spawnJvm(key, classpath, jvmOptions, runnerClass)
          case None =>
            spawnJvm(key, classpath, jvmOptions, runnerClass)
        }
      } yield jvm

    private def spawnJvm(
        key: JvmKey,
        classpath: List[Path],
        jvmOptions: List[String],
        runnerClass: String
    ): IO[ManagedJvm] = IO
      .blocking {
        val javaPath = jvmCommand
        val cpString = classpath.map(_.toString).mkString(File.pathSeparator)

        val cmd = List(javaPath.toString) ++ jvmOptions ++ List(
          "-cp",
          cpString,
          runnerClass
        )

        val pb = new ProcessBuilder(cmd: _*)
        pb.redirectErrorStream(false)

        val process = pb.start()
        val stdin = new PrintWriter(new BufferedOutputStream(process.getOutputStream), true)
        val stdout = new BufferedReader(new InputStreamReader(process.getInputStream))
        val stderr = new BufferedReader(new InputStreamReader(process.getErrorStream))

        new ManagedJvm(process, stdin, stdout, stderr, key)
      }
      .flatTap(jvm => allJvms.update(_ + jvm))
      .flatTap(waitForReady)

    private def waitForReady(jvm: ManagedJvm): IO[Unit] =
      IO.blocking {
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

    override def size: IO[Int] =
      allJvms.get.map(_.size)

    /** Return a JVM to the pool for reuse */
    private def release(jvm: ManagedJvm): IO[Unit] =
      if (jvm.isAlive) {
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
        // Dead JVM, just remove from tracking
        allJvms.update(_ - jvm)
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
          IO.blocking {
            val line = jvm.stdout.readLine()
            if (line == null) {
              jvm.markDead()
              None
            } else {
              TestProtocol.decodeResponse(line) match {
                case Right(response) => Some(response)
                case Left(err) =>
                  Some(
                    TestProtocol.TestResponse.Error(
                      s"Protocol error: ${err.getMessage}",
                      Some(s"Line: $line")
                    )
                  )
              }
            }
          }
        }.unNoneTerminate

      override def getThreadDump: IO[Option[TestProtocol.TestResponse.ThreadDump]] =
        for {
          _ <- sendCommand(TestProtocol.TestCommand.GetThreadDump)
          response <- IO
            .blocking {
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

      override def isAlive: IO[Boolean] =
        IO(jvm.isAlive)

      override def kill: IO[Unit] =
        IO(jvm.kill())
    }
  }
}
