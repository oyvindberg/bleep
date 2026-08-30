package bleep.bsp

import bleep.bsp.TestRunnerTypes.{TerminationReason, TestEventHandler, TestResult, TestSuite}
import bleep.bsp.protocol.{OutputChannel, TestStatus}

import java.nio.file.{Files, Path}

/** Drives an `sbt.testing.Framework` and translates what it reports into bleep's test events.
  *
  * Scala.js and Scala Native both run their tests by obtaining a real `sbt.testing.Framework` from a platform adapter — `TestAdapter` on either side — and from
  * that point on the work is identical to what the JVM runner does: build task definitions, execute them, map `sbt.testing.Event`s onto bleep's handler. Only
  * the way the framework is acquired differs. Keeping the shared half here means a defect in it is fixed once, and a framework that works on one platform is
  * not one code path away from failing on the other.
  *
  * The classloader-crossing part is deliberately not here. Adapters live in per-version classloaders, so acquiring the framework is reflective and stays with
  * the platform that knows which artifact to resolve. `sbt.testing.*` is delegated to bleep's own classloader (see `CompilerResolver.CompilerTopLoader`), so
  * the `Framework` handed to this object is directly assignment-compatible and needs no reflection at all.
  */
object SbtTestingBridge {

  /** Framework implementations to offer a platform adapter, in the order they are tried.
    *
    * An adapter loads whichever of these the linked artifact actually contains and reports the rest as absent, so naming a framework here costs nothing when it
    * is missing. There is no per-framework code anywhere behind this list: everything else goes through `sbt.testing`.
    *
    * Kept in step with `ClasspathTestDiscovery.knownFrameworks`, which decides what gets discovered in the first place. A framework present there and missing
    * here is discovered and then cannot be run — which is what limited Scala Native to munit, ScalaTest and utest while every other platform ran more.
    */
  val knownFrameworkClassNames: List[String] = List(
    "munit.Framework",
    "org.scalatest.tools.Framework",
    "utest.runner.Framework",
    "org.scalacheck.ScalaCheckFramework",
    "org.specs2.runner.Specs2Framework",
    "zio.test.sbt.ZTestFramework",
    "weaver.framework.CatsEffect",
    "hedgehog.sbt.Framework",
    "minitest.runner.Framework",
    "com.github.sbt.junit.jupiter.api.JupiterFramework"
  )

  /** Build task definitions for `suites` and run them.
    *
    * Fingerprints come from the framework itself. A fabricated fingerprint is not a neutral placeholder: it is what tells a framework whether a suite is a
    * class to instantiate or a module to load, and `SubclassFingerprint(superclassName = "java.lang.Object", isModule = false)` — which the Scala Native runner
    * passed — describes munit's shape but not utest's, and matches no framework's own declaration. Asking the framework, then keeping the first fingerprint
    * that actually yields tasks, uses only metadata the framework published about itself.
    *
    * @throws RuntimeException
    *   when no fingerprint produces a task. That means the suite cannot be run, and reporting it as a pass — or as a suite that ran zero tests — is how a
    *   broken runner stays invisible.
    */
  def runSuites(
      framework: sbt.testing.Framework,
      suites: List[TestSuite],
      eventHandler: TestEventHandler,
      loader: ClassLoader,
      suiteIsModule: String => Boolean
  ): TestResult = {
    val runner = framework.runner(Array.empty[String], Array.empty[String], loader)
    val tasks = tasksFor(framework, runner, suites, suiteIsModule)

    // Per-suite counters, keyed by the suite's fully qualified name.
    val suiteCounts = scala.collection.mutable.LinkedHashMap.empty[String, Counts]
    def bump(suite: String)(f: Counts => Counts): Unit =
      suiteCounts.update(suite, f(suiteCounts.getOrElse(suite, Counts.zero)))

    // The suite whose task is currently executing. It attributes both events and framework logging: `sbt.testing.Logger` carries no suite of its own, and
    // output attributed to "" is dropped at the reporting layer, which is exactly where a failing suite's explanation belongs.
    var currentSuite: String = suites.headOption.map(_.fullyQualifiedName).getOrElse("")

    val sbtEventHandler = new sbt.testing.EventHandler {
      def handle(event: sbt.testing.Event): Unit = {
        // Attributed to the suite whose task is executing, not to `event.fullyQualifiedName()`. Frameworks differ on what they put there: munit reports the
        // test's own fully qualified name, so keying on it invents one suite per test, and the reporting layer then shows a suite that failed for every failing
        // test on top of the real one. The executing suite is unambiguous because bleep runs one suite per task.
        val suiteName = currentSuite
        val testName = event.selector() match {
          case ts: sbt.testing.TestSelector       => ts.testName()
          case ns: sbt.testing.NestedTestSelector => ns.testName()
          case _                                  => event.fullyQualifiedName()
        }

        eventHandler.onTestStarted(suiteName, testName)

        val status = event.status() match {
          case sbt.testing.Status.Success  => bump(suiteName)(_.passed); TestStatus.Passed
          case sbt.testing.Status.Failure  => bump(suiteName)(_.failed); TestStatus.Failed
          case sbt.testing.Status.Error    => bump(suiteName)(_.failed); TestStatus.Error
          case sbt.testing.Status.Skipped  => bump(suiteName)(_.skipped); TestStatus.Skipped
          case sbt.testing.Status.Ignored  => bump(suiteName)(_.ignored); TestStatus.Ignored
          case sbt.testing.Status.Canceled => bump(suiteName)(_.skipped); TestStatus.Cancelled
          case sbt.testing.Status.Pending  => bump(suiteName)(_.skipped); TestStatus.Pending
        }

        val thrown = Option(event.throwable()).filter(_.isDefined).map(_.get())
        // Both, and from the same throwable: the message is what the exception said, the rendering is the whole of it. Sending only the first is what made a
        // ScalaTest assertion failure — whose message is null — arrive as a failure with nothing to read.
        eventHandler.onTestFinished(suiteName, testName, status, event.duration(), thrown.flatMap(t => Option(t.getMessage)), thrown.map(renderThrowable))
      }
    }

    val sbtLoggers = Array[sbt.testing.Logger](new sbt.testing.Logger {
      // Reported as unsupported so frameworks emit plain text. Colour codes here would reach JUnit XML, where ESC is not a legal character at all.
      def ansiCodesSupported(): Boolean = false
      def error(msg: String): Unit = eventHandler.onOutput(currentSuite, msg, OutputChannel.Stderr)
      def warn(msg: String): Unit = eventHandler.onOutput(currentSuite, msg, OutputChannel.Stdout)
      def info(msg: String): Unit = eventHandler.onOutput(currentSuite, msg, OutputChannel.Stdout)
      def debug(msg: String): Unit = ()
      def trace(t: Throwable): Unit = eventHandler.onOutput(currentSuite, t.toString, OutputChannel.Stderr)
    })

    val startedSuites = scala.collection.mutable.LinkedHashSet.empty[String]
    val requested = suites.map(_.fullyQualifiedName).toSet

    // A task's nested tasks belong to the same suite it does. Their task definitions do not reliably say so — munit's name each individual test — so the owning
    // suite is carried down explicitly rather than re-read at each level.
    def execute(task: sbt.testing.Task, owningSuite: String): Unit = {
      currentSuite = owningSuite
      if (startedSuites.add(owningSuite)) eventHandler.onSuiteStarted(owningSuite)
      task.execute(sbtEventHandler, sbtLoggers).foreach(execute(_, owningSuite))
      currentSuite = owningSuite
    }
    tasks.foreach { task =>
      val declared = task.taskDef().fullyQualifiedName()
      // Prefer the name bleep asked for; a framework is free to report tasks under a name of its own choosing.
      val owningSuite = if (requested.contains(declared)) declared else suites.headOption.map(_.fullyQualifiedName).getOrElse(declared)
      execute(task, owningSuite)
    }

    // Every suite asked for must be reported, whether or not the framework produced events for it. A suite that finishes silently is still a suite that ran no
    // tests, and the reporting layer can only say so if it hears about it.
    suites.map(_.fullyQualifiedName).foreach(name => if (startedSuites.add(name)) eventHandler.onSuiteStarted(name))
    startedSuites.foreach { name =>
      val c = suiteCounts.getOrElse(name, Counts.zero)
      eventHandler.onSuiteFinished(name, c.passedCount, c.failedCount, c.skippedCount)
    }

    runner.done(): Unit

    val total = suiteCounts.values.foldLeft(Counts.zero)((a, b) => a.combine(b))
    TestResult(total.passedCount, total.failedCount, total.skippedCount, total.ignoredCount, TerminationReason.Completed)
  }

  /** Ask the framework for task definitions covering `suites`, trying each fingerprint it declares.
    *
    * Fingerprints whose `isModule` matches what the suite actually is are tried first. "First fingerprint that yields a task" alone is not enough: a framework
    * is free to hand back a task for a shape the suite does not have and only fail when it tries to load it, and several declare more than one — ScalaCheck
    * declares four. An object suite matched against a class fingerprint produced `ClassNotFoundException: example.ScalacheckFixture`, naming the class form of
    * something that only exists as `example.ScalacheckFixture$`.
    */
  /** Render a failure's throwable the way a person needs to read it: what was thrown, what it said, and where.
    *
    * This used to be `getMessage` alone, and that is never enough. A null `getMessage` is not the absence of information — it is one field of an exception that
    * also has a type and a stack — and reading only that field made real failures arrive as empty ones. ScalaTest on Scala.js was the clearest case: its
    * `TestFailedException` carries the explanation in the exception *type* and the frames while `getMessage` is null, so a failing assertion reached the report
    * saying nothing at all. Both are recoverable, and both are reported now.
    *
    * `toString` rather than `getClass.getName`, deliberately. Across the Scala.js and Scala Native adapters a throwable is rehydrated into a synthetic subclass
    * of the serializer's own making, so its runtime class name is meaningless — but `toString` is carried over from the original and still reads
    * `org.scalatest.exceptions.TestFailedException`.
    */
  private[bsp] def renderThrowable(t: Throwable): String = {
    // Capped: a deep stack in a report helps nobody, and what matters is at the top.
    val frames = t.getStackTrace.take(30).map(frame => s"  at $frame").toList
    (String.valueOf(t.toString) :: frames).mkString("\n")
  }

  private def tasksFor(
      framework: sbt.testing.Framework,
      runner: sbt.testing.Runner,
      suites: List[TestSuite],
      suiteIsModule: String => Boolean
  ): Array[sbt.testing.Task] =
    if (suites.isEmpty) runner.tasks(Array.empty)
    else {
      val declared = framework.fingerprints()
      if (declared.isEmpty)
        throw new RuntimeException(s"Test framework ${framework.name()} declares no fingerprints, so ${suites.size} suite(s) cannot be run")

      // Ordered against the first suite; bleep runs one suite per task, so there is only ever one to agree with.
      val wantModule = suites.headOption.exists(s => suiteIsModule(s.fullyQualifiedName))
      val (matching, rest) = declared.toList.partition { fp =>
        fingerprintIsModule(fp).contains(wantModule)
      }
      val fingerprints = matching ::: rest

      val attempts = fingerprints.iterator.map { fp =>
        val taskDefs = suites.map { suite =>
          new sbt.testing.TaskDef(suite.fullyQualifiedName, fp, false, Array(new sbt.testing.SuiteSelector))
        }.toArray
        runner.tasks(taskDefs)
      }
      attempts.find(_.nonEmpty).getOrElse {
        val names = suites.map(_.fullyQualifiedName).mkString(", ")
        throw new RuntimeException(
          s"Test framework ${framework.name()} produced no tasks for $names using any of its ${fingerprints.size} fingerprint(s). " +
            "The suite was discovered but cannot be executed."
        )
      }
    }

  /** Null-free view of a fingerprint's module-ness; `None` when the kind says nothing about it. */
  private def fingerprintIsModule(fp: sbt.testing.Fingerprint): Option[Boolean] =
    fp match {
      case sfp: sbt.testing.SubclassFingerprint  => Some(sfp.isModule)
      case afp: sbt.testing.AnnotatedFingerprint => Some(afp.isModule)
      case _                                     => None
    }

  /** Decide whether a suite is a Scala object by looking for the class file the compiler emits for one.
    *
    * Reading the classpath rather than loading the class: these are Scala.js and Scala Native outputs, whose `.class` files exist for separate compilation but
    * name platform types this JVM has no business resolving. The presence of `Foo$.class` is the same evidence a classloader would use, without the risk.
    */
  def moduleDetector(classpath: List[Path]): String => Boolean = { fqn =>
    val relative = fqn.replace('.', '/') + "$.class"
    classpath.exists { entry =>
      if (Files.isDirectory(entry)) Files.exists(entry.resolve(relative))
      else if (Files.isRegularFile(entry) && entry.getFileName.toString.endsWith(".jar"))
        try {
          val zip = new java.util.zip.ZipFile(entry.toFile)
          try zip.getEntry(relative) != null
          finally zip.close()
        } catch { case _: Throwable => false }
      else false
    }
  }

  private case class Counts(passedCount: Int, failedCount: Int, skippedCount: Int, ignoredCount: Int) {
    def passed: Counts = copy(passedCount = passedCount + 1)
    def failed: Counts = copy(failedCount = failedCount + 1)
    def skipped: Counts = copy(skippedCount = skippedCount + 1)
    def ignored: Counts = copy(ignoredCount = ignoredCount + 1)
    def combine(other: Counts): Counts =
      Counts(passedCount + other.passedCount, failedCount + other.failedCount, skippedCount + other.skippedCount, ignoredCount + other.ignoredCount)
  }

  private object Counts {
    val zero: Counts = Counts(0, 0, 0, 0)
  }

  /** Reflective construction of Scala collections inside an adapter's classloader.
    *
    * A thin naming layer over [[AlienValue]], which is where the actual reflection and its hazards live. Kept because these call sites read better as
    * `toMap`/`toList` than as `AlienMap.of(...).underlying`, and because everything here hands the raw object straight back to a reflective `invoke`.
    */
  object ScalaColl {
    def toMap(entries: Map[String, String], loader: ClassLoader): AnyRef = AlienMap.of(entries, loader).underlying

    def toList(elems: List[Any], loader: ClassLoader): AnyRef = AlienList.of(elems.map(_.asInstanceOf[AnyRef]), loader).underlying

    def fromList[A](scalaList: Any, loader: ClassLoader): List[A] =
      AlienList(scalaList.asInstanceOf[AnyRef], loader).elements.map(_.asInstanceOf[A])

    def fromOption[A](scalaOption: Any, loader: ClassLoader): Option[A] = AlienOption(scalaOption.asInstanceOf[AnyRef], loader).as[A]
  }
}
