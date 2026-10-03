package bleep.analysis

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.jdk.OptionConverters.*

/** ECJ's problems reach the reporter as ECJ's own objects, with the column and offsets the text output never carried. */
class EcjProblemReportingTest extends AnyFunSuite with Matchers {

  private val ecjVersion = "3.40.0"

  private def withEcjClassLoader(f: ClassLoader => Unit): Unit = {
    val dep = bleep.model.Dep.Java("org.eclipse.jdt", "ecj", ecjVersion)
    val ecjJars = dep.asJava(bleep.model.VersionCombo.Java) match {
      case Right(javaDep) => coursier.Fetch().addDependencies(javaDep.dependency).run().map(_.toPath)
      case Left(e)        => throw new RuntimeException(s"Failed to resolve ECJ $ecjVersion: $e")
    }
    val ecjClassLoader = new java.net.URLClassLoader(ecjJars.map(_.toUri.toURL).toArray, getClass.getClassLoader)
    try f(ecjClassLoader)
    finally ecjClassLoader.close()
  }

  private val noLog: xsbti.Logger = new xsbti.Logger {
    def debug(msg: java.util.function.Supplier[String]): Unit = ()
    def error(msg: java.util.function.Supplier[String]): Unit = ()
    def info(msg: java.util.function.Supplier[String]): Unit = ()
    def trace(err: java.util.function.Supplier[Throwable]): Unit = ()
    def warn(msg: java.util.function.Supplier[String]): Unit = ()
  }

  /** Compiles `sources` (file name -> content) with ECJ, returning whether it succeeded, the reporter, and where each source was written. */
  private def compile(
      ecjClassLoader: ClassLoader,
      options: Array[String],
      sources: Map[String, String]
  ): (Boolean, CollectingReporter, Map[String, Path]) = {
    val dir = Files.createTempDirectory("ecj-problems")
    val written = sources.map { case (name, content) =>
      val file = dir.resolve("src").resolve(name)
      Files.createDirectories(file.getParent)
      Files.writeString(file, content)
      name -> file
    }
    val reporter = new CollectingReporter
    val success = new EcjCompiler(ecjClassLoader, CancellationToken.never, ProgressListener.noop).run(
      written.values.map(p => sbt.internal.inc.PlainVirtualFile(p): xsbti.VirtualFile).toArray,
      options,
      sbt.internal.inc.CompileOutput(dir.resolve("classes")),
      xsbti.compile.IncToolOptionsUtil.defaultIncToolOptions(),
      reporter,
      noLog
    )
    (success, reporter, written)
  }

  test("an error and a warning arrive with their file, line, column and offsets") {
    withEcjClassLoader { ecjClassLoader =>
      val source =
        """package p;
          |
          |public class Broken {
          |    int f() { return undefinedName; }
          |}
          |""".stripMargin
      // in its own file: ECJ does not report unused imports in a file that has errors
      val warned =
        """package p;
          |import java.util.List;
          |public class Warned {}
          |""".stripMargin
      val (success, reporter, written) =
        compile(ecjClassLoader, Array("-source", "17", "-target", "17"), Map("p/Broken.java" -> source, "p/Warned.java" -> warned))

      success shouldBe false
      val problems = reporter.problems().toList
      val error = problems.find(_.severity == xsbti.Severity.Error).getOrElse(fail(s"no error among $problems"))
      val warning = problems.find(_.severity == xsbti.Severity.Warn).getOrElse(fail(s"no warning among $problems"))

      error.message() should include("undefinedName")
      Path.of(error.position().sourcePath().get) shouldBe written("p/Broken.java").toRealPath()
      error.position().line().toScala shouldBe Some(4)
      val start = source.indexOf("undefinedName")
      error.position().startOffset().toScala shouldBe Some(start)
      error.position().endOffset().toScala shouldBe Some(start + "undefinedName".length)
      error.position().pointer().toScala shouldBe Some("    int f() { return ".length)

      warning.message() should include("java.util.List")
      warning.position().line().toScala shouldBe Some(2)
      Path.of(warning.position().sourcePath().get) shouldBe written("p/Warned.java").toRealPath()

      problems.size shouldBe 2
    }
  }

  test("a failure without a problem, such as an invalid option, is reported with ECJ's output") {
    withEcjClassLoader { ecjClassLoader =>
      val (success, reporter, _) = compile(ecjClassLoader, Array("-no-such-option"), Map("p/Fine.java" -> "package p; public class Fine {}"))

      success shouldBe false
      val problems = reporter.problems().toList
      problems.map(_.severity) shouldBe List(xsbti.Severity.Error)
      problems.head.message() should include("-no-such-option")
    }
  }

  test("a problem that belongs to no source, as an annotation processor reports, reaches the reporter") {
    withEcjClassLoader { ecjClassLoader =>
      val reporter = new CollectingReporter
      val problems = new EcjProblemReporter(ecjClassLoader, reporter)
      val out = new java.io.PrintWriter(new java.io.ByteArrayOutputStream())
      val main = EcjCompiler.createMainWithoutProgress(ecjClassLoader, out, out, problems)

      val defaultProblem = ecjClassLoader
        .loadClass("org.eclipse.jdt.internal.compiler.problem.DefaultProblem")
        .getConstructor(
          classOf[Array[Char]],
          classOf[String],
          classOf[Int],
          classOf[Array[String]],
          classOf[Int],
          classOf[Int],
          classOf[Int],
          classOf[Int],
          classOf[Int]
        )
      val warningSeverity = 0 // ProblemSeverities.Warning
      val problem = defaultProblem.newInstance(
        null,
        "a processor's note",
        Integer.valueOf(0),
        Array.empty[String],
        Integer.valueOf(warningSeverity),
        Integer.valueOf(-1),
        Integer.valueOf(-1),
        Integer.valueOf(0),
        Integer.valueOf(0)
      )

      problems.onExtraProblems.accept(main, java.util.List.of(problem))

      val reported = reporter.problems().toList
      reported.map(p => (p.severity, p.message())) shouldBe List((xsbti.Severity.Warn, "a processor's note"))
      reported.head.position().sourcePath().toScala shouldBe None
      reported.head.position().line().toScala shouldBe None
      reported.head.position().offset().toScala shouldBe None
    }
  }
}
