package bleep.testing

/** Hides the frames a test failure's stack trace ends in that belong to the test framework rather than to the person reading it.
  *
  * A failure's trace is a sandwich: the frames of your code, and underneath them every layer that had to run to get there — the framework's executor, its
  * interceptors, the reflection that constructed the suite, the JDK, and finally bleep's own runner. Only the top is evidence. The rest is the same on every
  * failure in the project and says nothing about this one.
  *
  * The proportions are not marginal. A kotest suite whose constructor throws arrives with 120 frames, three of which are yours. JUnit 3 reports the same
  * fifty-eight-frame trace once per test method. Measured across the 32 (framework, target) pairs the matrix runs, 89% of all frames reaching the console were
  * framework machinery.
  *
  * ==What is cut==
  *
  * Only a trailing run, and only frames whose owner is recognised. Scanning upward from the last frame, each frame is dropped while it belongs to a known
  * framework, runtime or bridge; the first frame that is not recognised stops the cut and everything above it is printed untouched. That direction matters:
  *   - A framework frame *above* your code is kept, because it is part of the call chain you care about — `munit.FunSuite.assertEquals` names the assertion
  *     that failed, and `io.kotest.core.spec.style.FunSpec.<init>` sits between two of your own frames.
  *   - A frame this table does not know is treated as yours. Being unrecognised is never a reason to hide something.
  *
  * Each `Caused by:` section is cut independently, since each has its own tail of machinery. That is what turns kotest's 44-line trace into 7 lines whose last
  * three are the constructor that actually threw.
  *
  * ==What is not cut==
  *
  * Frames that carry no package at all. Cucumber reports its steps as `at ✽.the assertion fails(classpath:example/fixture.feature:13)` and ZIO renders a source
  * path rather than a class; neither matches anything here, so both survive in full — which is correct, because in both cases those frames *are* the user's
  * code.
  *
  * The one exception is Scala.js, where the linker emits `{anonymous}` for every lambda. Such a frame carries nothing but an offset into `main.js` and cannot
  * be attributed to anyone, so it does not stop a cut — but it is never the reason for one either.
  *
  * ==Where the full trace still lives==
  *
  * This runs at presentation time only. The JUnit XML report ([[JUnitXmlReport]]) is written from the unmodified throwable, so CI and any tool reading the
  * report still get every frame.
  */
object StackTraceElision {

  /** A layer that can appear beneath a test failure, and the packages it owns.
    *
    * The label is what the elision marker prints, so it is the name a user would recognise ("junit", not "org.junit"). Grouping several prefixes under one
    * label is deliberate: JUnit spans three unrelated package roots across its three major versions, and a reader does not care which.
    */
  final case class Owner(label: String, prefixes: List[String])

  /** The frameworks bleep's matrix runs, each owning its own package root.
    *
    * Determined per (framework, target) by reading the traces of real failing runs on every platform the framework supports — see
    * `bleep.PlatformFrameworkHarness`, which asserts these against fresh output on every build. Where a framework's machinery differs by target it is the same
    * package root on all of them; what differs is the *bridge* underneath it, and those are listed separately below.
    */
  private val frameworks: List[Owner] = List(
    Owner("munit", List("munit.")),
    Owner("scalatest", List("org.scalatest.")),
    Owner("scalacheck", List("org.scalacheck.")),
    Owner("utest", List("utest.")),
    Owner("minitest", List("minitest.")),
    Owner("weaver", List("weaver.")),
    Owner("specs2", List("org.specs2.")),
    Owner("hedgehog", List("hedgehog.")),
    Owner("zio-test", List("zio.")),
    // JUnit 3's `junit.framework`, JUnit 4's `org.junit.runners`, and the JUnit 5 platform all reach the console through the same runner.
    Owner("junit", List("org.junit.", "junit.framework.", "junit.textui.")),
    Owner("kotest", List("io.kotest.")),
    Owner("spek", List("org.spekframework.")),
    Owner("jqwik", List("net.jqwik.")),
    Owner("cucumber", List("io.cucumber.")),
    Owner("testng", List("org.testng."))
  )

  /** Everything between a framework and the operating system: bleep's own runner, the sbt test-interface, the Scala.js and Scala Native test bridges, and the
    * reflection layers each platform uses to instantiate a suite.
    */
  private val bridges: List[Owner] = List(
    // `mill.testng` is the TestNG bridge bleep liberated from Mill — TestNG ships no sbt test-interface implementation of its own. It reads as bleep's
    // plumbing from where a user sits, and leaving it out stopped the cut dead: one unrecognised frame in the middle of a TestNG trace kept all twenty-seven
    // `org.testng` frames above it on screen.
    Owner("bleep", List("bleep.testing.runner.", "mill.testng.")),
    Owner("sbt", List("sbt.testing.")),
    Owner("scala.js", List("org.scalajs.testing.", "org.scalajs.jsenv.", "scala.scalajs.")),
    Owner("scala native", List("scala.scalanative.")),
    Owner("reflect", List("org.portablescala.reflect.")),
    // weaver's traces are cats-effect fiber traces, so its plumbing shows up under both names.
    Owner("cats-effect", List("cats.effect.", "fs2."))
  )

  /** Language and platform runtimes. Broad on purpose: nobody's test lives in `java.`, `scala.` or `kotlin.`, so matching the whole root costs nothing and
    * catches the collection, coroutine and future machinery that every framework threads its callbacks through.
    */
  private val runtimes: List[Owner] = List(
    Owner("jdk", List("java.", "javax.", "jdk.", "sun.")),
    Owner("scala", List("scala.")),
    Owner("kotlin", List("kotlin.", "kotlinx.")),
    Owner("node", List("<jscode>.process.", "process."))
  )

  private val owners: List[Owner] = frameworks ::: bridges ::: runtimes

  /** A Scala.js frame the linker could not name. Not attributable to anyone, so it neither stops a cut nor causes one. */
  private val Unattributable = "linker"

  /** The JVM's own back-reference (`... 27 more`), which stands for frames identical to the enclosing trace's. When that enclosing tail is elided the
    * back-reference points at nothing, so it goes with it.
    */
  private val BackReference = "jvm"

  /** `at java.base/java.lang.Class.newInstance(Class.java:715)` — module prefix on JDK 9+, and weaver's `at flatMap @ weaver.…` fiber-trace form, which puts
    * the operation before an `@` and the class after it.
    */
  private val FramePattern = """^\s*at\s+(?:\S+\s+@\s+)?(?:[A-Za-z0-9_.$]+/)?(.*)$""".r

  /** A Kotlin/Native frame, which is symbolicated rather than named: `at 5  mytest.kexe  0x10037dc4b  kfun:kotlin.native.internal.test.TestCase#run(){} + 227`.
    * The symbol sits between the address and the offset.
    */
  private val NativeFramePattern = """^\s*at\s+\d+\s+\S+\s+0x[0-9a-fA-F]+\s+(.+?)\s*\+\s*\d+\s*$""".r

  /** Which layer a frame belongs to, or `None` when it is not one this table knows — which is how a frame gets treated as the user's own. */
  private def ownerOf(rawLine: String): Option[String] =
    plain(rawLine) match {
      case NativeFramePattern(symbol) =>
        // Kotlin compiles every declaration you write to a `kfun:` symbol, so a symbol without that prefix — `Konan_start`, `Init_and_run_start`, `Konan_main`
        // — is the Kotlin/Native runtime starting your program and cannot be yours.
        if (!symbol.startsWith("kfun:")) Some("kotlin/native")
        else {
          val fqn = symbol.stripPrefix("kfun:")
          owners.collectFirst { case Owner(label, prefixes) if prefixes.exists(fqn.startsWith) => label }
        }
      case FramePattern(rest) =>
        // Scala.js prefixes linked frames with `<jscode>.`; the name underneath it is an ordinary one and matches the table like any other.
        val name = rest.stripPrefix("<jscode>.")
        if (name.startsWith("{anonymous}")) Some(Unattributable)
        else owners.collectFirst { case Owner(label, prefixes) if prefixes.exists(name.startsWith) => label }
      case BareFramePattern(name) =>
        owners.collectFirst { case Owner(label, prefixes) if prefixes.exists(name.startsWith) => label }
      case _ => None
    }

  /** A frame with no `at ` in front of it: `utest.asserts.Asserts$.assertImpl(Asserts.scala:30)`.
    *
    * Several frameworks print their own report to stdout in this shape — utest and ScalaTest both do — and bleep shows that captured output underneath its own
    * report of the same failure. Without this the frames bleep had just cut came straight back, in full, three lines below the cut.
    *
    * Anchored at both ends and requiring a dotted path followed by a parenthesised location, so ordinary printed output cannot be mistaken for one. A line that
    * does match still has to belong to a package in the table before anything is hidden.
    */
  private val BareFramePattern = """^\s*([A-Za-z_$][\w$<>]*(?:\.[\w$<>]+)+)\([^()]*\)\s*$""".r

  /** Colour has to come off before anything is matched, and stay on in what is printed.
    *
    * A framework's own report is captured with its escapes intact — that is the point of capturing it — and every pattern here is anchored, so a red frame
    * begins with `\u001b[31m` and matches nothing. This strips for the decision only; the line that reaches the terminal is the original.
    */
  private val Ansi = """\u001b\[[0-9;]*[A-Za-z]""".r

  private def plain(line: String): String = Ansi.replaceAllIn(line, "")

  private def isFrameLine(line: String): Boolean = {
    val trimmed = plain(line).trim
    trimmed.startsWith("at ") || trimmed.startsWith("... ") || BareFramePattern.matches(plain(line))
  }

  private def isBackReference(line: String): Boolean = plain(line).trim.startsWith("... ")

  /** A frame proper, as opposed to a `... N more` back-reference or an elision marker. Both shapes count: with `at ` in front and without. */
  private def isRealFrame(line: String): Boolean = {
    val p = plain(line)
    p.trim.startsWith("at ") || BareFramePattern.matches(p)
  }

  /** Cut the trailing run of machinery off one block of consecutive frames. Returns the frames to keep, how many real frames went, and the layers they came
    * from in the order they appeared.
    */
  private def elideBlock(frames: List[String]): (List[String], Int, List[String]) = {
    val arr = frames.toArray
    var cut = arr.length
    var i = arr.length - 1
    var scanning = true
    val hit = List.newBuilder[String]
    while (scanning && i >= 0)
      if (isBackReference(arr(i))) {
        hit += BackReference
        cut = i
        i -= 1
      } else
        ownerOf(arr(i)) match {
          case Some(label) =>
            hit += label
            cut = i
            i -= 1
          case None => scanning = false
        }
    val labels = hit.result().reverse.filterNot(l => l == Unattributable || l == BackReference).distinct
    // Nothing identified means nothing to hide. A tail of unnamed Scala.js frames and JVM back-references is not evidence of machinery, and cutting it would
    // be hiding frames while unable to say whose they are — the one thing this must never do.
    if (cut == arr.length || labels.isEmpty) (frames, 0, Nil)
    else (frames.take(cut), frames.drop(cut).count(isRealFrame), labels)
  }

  private def marker(indent: String, dropped: Int, labels: List[String]): String = {
    val plural = if (dropped == 1) "frame" else "frames"
    val from = if (labels.isEmpty) "" else labels.mkString(" (", ", ", ")")
    s"$indent... $dropped $plural elided$from"
  }

  /** Leading whitespace of a frame line, so the marker lines up with the frames it replaces. */
  private def indentOf(line: String): String = {
    val idx = line.indexWhere(!_.isWhitespace)
    if (idx > 0) line.substring(0, idx) else ""
  }

  /** Elide framework machinery from a rendered stack trace, returning the lines to print.
    *
    * If the cut would leave the trace with no frames at all, nothing is cut. Hiding noise is only worth doing when there is signal underneath it, and a failure
    * rendered as `java.lang.Throwable:` followed by `... 30 frames elided` tells you strictly less than the thirty frames did — ScalaTest on Scala Native hands
    * over an empty exception built at its own reporter's call site, and cutting that to nothing turns a bad report into no report.
    *
    * The test is on the trace as a whole, not on each `Caused by:` section. kotest's constructor failure is three sections of which only the last holds your
    * frames; cutting the first two is exactly right, and a per-section rule would put all thirty-six back.
    */
  def elide(stackTrace: String): List[String] = {
    val cut = elideBlocks(stackTrace)
    if (cut.exists(isRealFrame)) cut else stackTrace.split("\n").toList
  }

  private def elideBlocks(stackTrace: String): List[String] = {
    val result = List.newBuilder[String]
    var block = List.newBuilder[String]
    var hasBlock = false

    def flush(): Unit =
      if (hasBlock) {
        val frames = block.result()
        val (kept, dropped, labels) = elideBlock(frames)
        result ++= kept
        if (dropped > 0) result += marker(indentOf(frames.headOption.getOrElse("")), dropped, labels)
        block = List.newBuilder[String]
        hasBlock = false
      }

    stackTrace.split("\n").foreach { line =>
      if (isFrameLine(line)) {
        block += line
        hasBlock = true
      } else {
        flush()
        result += line
      }
    }
    flush()
    result.result()
  }
}
