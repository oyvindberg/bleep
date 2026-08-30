package bleep.testing

import org.scalactic.TripleEqualsSupport
import org.scalatest.funsuite.AnyFunSuite

/** Every trace here was copied out of a real failing run of the framework named, on the target named. None of them are invented — inventing a stack trace is
  * how you write an elision rule that works on nothing.
  */
class StackTraceElisionTest extends AnyFunSuite with TripleEqualsSupport {

  test("a trace of only your own frames passes through untouched") {
    val input =
      """java.lang.RuntimeException: boom
        |	at example.Fixture.doWork(Fixture.scala:10)
        |	at example.Fixture.test(Fixture.scala:20)""".stripMargin

    assert(StackTraceElision.elide(input) === input.split("\n").toList)
  }

  test("munit on the JVM: a throwing constructor keeps the one frame that is yours") {
    val input =
      """java.lang.RuntimeException: ctor boom
        |	at example.CtorBoomMunitFixture.<init>(CtorBoomMunitFixture.scala:4)
        |	at java.base/jdk.internal.reflect.DirectConstructorHandleAccessor.newInstance(DirectConstructorHandleAccessor.java:62)
        |	at java.base/java.lang.reflect.Constructor.newInstanceWithCaller(Constructor.java:499)
        |	at java.base/java.lang.Class.newInstance(Class.java:715)
        |	at munit.MUnitRunner.<init>(MUnitRunner.scala:27)
        |	at munit.internal.junitinterface.JUnitComputer.getRunner(JUnitComputer.java:75)
        |	at org.junit.runners.model.RunnerBuilder.safeRunnerForClass(RunnerBuilder.java:70)
        |	at munit.internal.junitinterface.JUnitTask.execute(JUnitTask.java:64)
        |	at bleep.testing.runner.ForkedTestRunner.executeTasks(ForkedTestRunner.java:463)
        |	at bleep.testing.runner.ForkedTestRunner.main(ForkedTestRunner.java:129)""".stripMargin

    val result = StackTraceElision.elide(input)
    assert(result.head === "java.lang.RuntimeException: ctor boom")
    assert(result(1) === "\tat example.CtorBoomMunitFixture.<init>(CtorBoomMunitFixture.scala:4)")
    assert(result(2) === "\t... 9 frames elided (jdk, munit, junit, bleep)")
    assert(result.sizeIs == 3)
  }

  test("a framework frame above your code survives — it names the assertion that failed") {
    val input =
      """munit.ComparisonFailException: values are not the same
        |	at munit.FunSuite.assertEquals(FunSuite.scala:12)
        |	at example.MunitFixture.$anonfun$new$5(MunitFixture.scala:6)
        |	at scala.runtime.java8.JFunction0$mcV$sp.apply(JFunction0$mcV$sp.scala:18)""".stripMargin

    val result = StackTraceElision.elide(input)
    assert(result(1) === "\tat munit.FunSuite.assertEquals(FunSuite.scala:12)")
    assert(result(2) === "\tat example.MunitFixture.$anonfun$new$5(MunitFixture.scala:6)")
    assert(result(3) === "\t... 1 frame elided (scala)")
  }

  test("kotest on the JVM: each `Caused by:` section is cut on its own") {
    val input =
      """io.kotest.engine.spec.SpecInstantiationException: Could not create instance of class example.CtorBoomKotestFixture
        |	at io.kotest.engine.spec.SpecInstantiator.instantiate-gIAlu-s(SpecInstantiator.kt:48)
        |	at kotlinx.coroutines.BuildersKt.runBlocking(Unknown Source)
        |	at io.kotest.runner.junit.platform.KotestJunitPlatformTestEngine.execute(KotestJunitPlatformTestEngine.kt:56)
        |Caused by: java.lang.reflect.InvocationTargetException
        |	at java.base/java.lang.reflect.Constructor.newInstance(Constructor.java:483)
        |	at kotlin.reflect.jvm.internal.KCallableImpl.call(KCallableImpl.kt:151)
        |	at io.kotest.engine.InstantiateKt.instantiateOrObject(instantiate.kt:30)
        |	... 27 more
        |Caused by: java.lang.RuntimeException: ctor boom
        |	at example.CtorBoomKotestFixture._init_$lambda$0(CtorBoomKotestFixture.kt:7)
        |	at io.kotest.core.spec.style.FunSpec.<init>(funSpec.kt:25)
        |	at example.CtorBoomKotestFixture.<init>(CtorBoomKotestFixture.kt:6)
        |	... 33 more""".stripMargin

    val result = StackTraceElision.elide(input)
    assert(
      result === List(
        "io.kotest.engine.spec.SpecInstantiationException: Could not create instance of class example.CtorBoomKotestFixture",
        "\t... 3 frames elided (kotest, kotlin)",
        "Caused by: java.lang.reflect.InvocationTargetException",
        "\t... 3 frames elided (jdk, kotlin, kotest)",
        "Caused by: java.lang.RuntimeException: ctor boom",
        "\tat example.CtorBoomKotestFixture._init_$lambda$0(CtorBoomKotestFixture.kt:7)",
        // A kotest frame between two of yours: kept, because the chain above it is still yours.
        "\tat io.kotest.core.spec.style.FunSpec.<init>(funSpec.kt:25)",
        "\tat example.CtorBoomKotestFixture.<init>(CtorBoomKotestFixture.kt:6)",
        // Kept: a bare back-reference is not a frame anyone owns, so it is not cut on its own.
        "\t... 33 more"
      )
    )
  }

  test("cucumber's gherkin steps are not frames of any framework, so nothing is cut") {
    val input =
      """java.lang.AssertionError: expected 5 but was 4
        |	at ✽.the assertion fails(classpath:example/fixture.feature:13)
        |	at ✽.the step throws(classpath:example/fixture.feature:17)""".stripMargin

    assert(StackTraceElision.elide(input) === input.split("\n").toList)
  }

  test("zio renders a source path rather than a class, and a path belongs to nobody but you") {
    val input =
      """java.lang.RuntimeException: boom
        |	at example.ZioTestFixture$.$anonfun$spec$8(ZioTestFixture.scala:10)
        |	at zio.test.sbt.ZTestEvent$.convertEvent(ZTestEvent.scala:34)
        |	at zio.internal.FiberRuntime.run(FiberRuntime.scala:137)
        |	at java.base/java.lang.Thread.run(Thread.java:1474)""".stripMargin

    val result = StackTraceElision.elide(input)
    assert(result(1) === "\tat example.ZioTestFixture$.$anonfun$spec$8(ZioTestFixture.scala:10)")
    assert(result(2) === "\t... 3 frames elided (zio-test, jdk)")
  }

  test("Scala.js: an unnamed linker frame neither stops a cut nor causes one") {
    val input =
      """java.lang.RuntimeException: ctor boom
        |	at example.CtorBoomSpecs2Fixture.<init>(/link-output/main.js:100560)
        |	at <jscode>.{anonymous}()(/link-output/main.js:100548)
        |	at scala.scalajs.reflect.InvokableConstructor.newInstance(/link-output/main.js:20185)
        |	at org.specs2.reflect.Classes.newInstance(/link-output/main.js:29298)
        |	at <jscode>.{anonymous}()(/link-output/main.js:62265)""".stripMargin

    val result = StackTraceElision.elide(input)
    assert(result(1) === "\tat example.CtorBoomSpecs2Fixture.<init>(/link-output/main.js:100560)")
    assert(result(2) === "\t... 4 frames elided (scala.js, specs2)")
  }

  test("a trace that is nothing but framework internals is left alone — cutting it would leave nothing at all") {
    val input =
      """org.scalatest.exceptions.TestFailedException
        |	at org.scalatest.Assertions.newAssertionFailedException(/link-output/main.js:27336)
        |	at org.scalatest.funsuite.AnyFunSuiteLike.runTest(/link-output/main.js:56088)""".stripMargin

    // ScalaTest on Scala Native hands over an empty exception built at its own reporter. Reduced to `java.lang.Throwable:` and a count of what was hidden, a
    // failure says less than the frames did.
    assert(StackTraceElision.elide(input) === input.split("\n").toList)
  }

  test("weaver's fiber traces put the class after an `@`, and are recognised there") {
    val input =
      """java.lang.RuntimeException: ctor boom
        |	at example.CtorBoomWeaverFixture$.<clinit>(CtorBoomWeaverFixture.scala:6)
        |	at weaver.internals.Reflection$.loadModule(Reflection.scala:77)
        |	at delay @ weaver.framework.WeaverFingerprints$$anon$1.apply(Fingerprints.scala:42)
        |	at flatMap @ weaver.framework.RunnerCompat$IOTask.run(RunnerCompat.scala:214)""".stripMargin

    val result = StackTraceElision.elide(input)
    assert(result(1) === "\tat example.CtorBoomWeaverFixture$.<clinit>(CtorBoomWeaverFixture.scala:6)")
    assert(result(2) === "\t... 3 frames elided (weaver)")
  }

  test("TestNG: one unrecognised bridge frame in the middle used to keep the whole trace on screen") {
    val input =
      """org.testng.TestNGException: Cannot instantiate class example.CtorBoomTestNGFixture
        |	at org.testng.TestRunner.init(TestRunner.java:302)
        |	at org.testng.TestNG.run(TestNG.java:1079)
        |	at mill.testng.TestNGTask.execute(TestNGRunner.java:37)
        |	at bleep.testing.runner.ForkedTestRunner.executeTasks(ForkedTestRunner.java:463)
        |	... 2 more
        |Caused by: java.lang.RuntimeException: ctor boom
        |	at example.CtorBoomTestNGFixture.<init>(CtorBoomTestNGFixture.java:8)
        |	at java.base/jdk.internal.reflect.DirectConstructorHandleAccessor.newInstance(DirectConstructorHandleAccessor.java:62)
        |	... 33 more""".stripMargin

    val result = StackTraceElision.elide(input)
    assert(
      result === List(
        "org.testng.TestNGException: Cannot instantiate class example.CtorBoomTestNGFixture",
        "\t... 4 frames elided (testng, bleep)",
        "Caused by: java.lang.RuntimeException: ctor boom",
        "\tat example.CtorBoomTestNGFixture.<init>(CtorBoomTestNGFixture.java:8)",
        "\t... 1 frame elided (jdk)"
      )
    )
  }

  test("Kotlin/Native frames are symbolicated, and `kfun:` is what separates your code from the runtime") {
    val input =
      """kotlin.RuntimeException: ctor boom
        |    at 0   mytest.kexe   0x10041e863   kfun:example.CtorBoomKotlinTestFixture#<init>(){} + 103
        |    at 2   mytest.kexe   0x10037b7d7   kfun:kotlin.native.internal.test.BaseClassSuite.TestCase#doRun(){} + 123
        |    at 9   mytest.kexe   0x10036d92f   kfun:kotlin.native.internal.test#main(kotlin.Array<kotlin.String>){} + 27
        |    at 10  mytest.kexe   0x10036dab3   Konan_start + 87
        |    at 11  mytest.kexe   0x10040d763   Init_and_run_start + 195""".stripMargin

    val result = StackTraceElision.elide(input)
    assert(result(1) === "    at 0   mytest.kexe   0x10041e863   kfun:example.CtorBoomKotlinTestFixture#<init>(){} + 103")
    assert(result(2) === "    ... 4 frames elided (kotlin, kotlin/native)")
  }

  test("Scala.js linked frames carry a `<jscode>.` prefix over an ordinary name") {
    val input =
      """java.lang.RuntimeException: boom
        |	at example.Fixture.test(/link-output/main.js:100)
        |	at <jscode>.scala.runtime.AbstractFunction0.apply(/link-output/main.js:21679)
        |	at org.scalajs.jsenv.nodejs.ComRun$$anon$1.run(ComSupport.scala:74)""".stripMargin

    val result = StackTraceElision.elide(input)
    assert(result(1) === "\tat example.Fixture.test(/link-output/main.js:100)")
    assert(result(2) === "\t... 2 frames elided (scala, scala.js)")
  }

  test("utest prints its own report to stdout with no `at ` in front of the frames, and it is cut the same way") {
    val input =
      """X example.CtorBoomUtestFixture 12ms
        |  java.lang.RuntimeException: ctor boom
        |    example.CtorBoomUtestFixture$.<clinit>(CtorBoomUtestFixture.scala:6)
        |    jdk.internal.misc.Unsafe.ensureClassInitialized0(Unsafe.java:-2)
        |    java.lang.reflect.Field.get(Field.java:438)
        |    utest.framework.PlatformShims$.loadModule(PlatformShims.scala:15)
        |    scala.Option.map(Option.scala:244)
        |    utest.runner.BaseRunner.$anonfun$3(BaseRunner.scala:121)""".stripMargin

    val result = StackTraceElision.elide(input)
    assert(
      result === List(
        "X example.CtorBoomUtestFixture 12ms",
        "  java.lang.RuntimeException: ctor boom",
        "    example.CtorBoomUtestFixture$.<clinit>(CtorBoomUtestFixture.scala:6)",
        "    ... 5 frames elided (jdk, utest, scala)"
      )
    )
  }

  test("ordinary printed output is not mistaken for a frame") {
    val input =
      """hello from the test
        |computed foo.bar for 3 items
        |a line with (parens) in it
        |Elapsed: 1.2s""".stripMargin

    assert(StackTraceElision.elide(input) === input.split("\n").toList)
  }

  test("a coloured frame is still a frame — the escapes come off for the decision, not for the output") {
    val red = "\u001b[31m"
    val reset = "\u001b[0m"
    val input =
      s"""java.lang.RuntimeException: boom
         |$red    example.UtestFixture$$.run(UtestFixture.scala:9)$reset
         |$red    utest.framework.PlatformShims$$.loadModule(PlatformShims.scala:15)$reset
         |$red    utest.runner.BaseRunner.$$anonfun$$3(BaseRunner.scala:121)$reset""".stripMargin

    val result = StackTraceElision.elide(input)
    assert(result.sizeIs == 3)
    // The surviving frame keeps its colour.
    assert(result(1) === s"$red    example.UtestFixture$$.run(UtestFixture.scala:9)$reset")
    assert(result(2).contains("... 2 frames elided (utest)"))
  }
}
