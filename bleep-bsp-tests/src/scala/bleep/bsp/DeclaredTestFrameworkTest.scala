package bleep.bsp

import bleep.model
import bleep.testing.FrameworkSelection
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import sbt.testing._

import java.io.File
import java.nio.file.{Files, Path, Paths}

/** A base class bleep has no knowledge of, so nothing but an explicit declaration can find its subclasses. */
class MarkerBase

class MarkerSuite extends MarkerBase

/** A minimal `sbt.testing.Framework` deliberately absent from `ClasspathTestDiscovery.knownFrameworks`. */
class MarkerFramework extends Framework {
  override def name(): String = "marker"

  override def fingerprints(): Array[Fingerprint] = Array(new SubclassFingerprint {
    override def superclassName(): String = classOf[MarkerBase].getName
    override def isModule(): Boolean = false
    override def requireNoArgConstructor(): Boolean = true
  })

  // Discovery only reads fingerprints; it never runs anything. If this is ever called the test is lying about what it covers.
  override def runner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader): Runner =
    throw new UnsupportedOperationException("MarkerFramework is a discovery fixture and cannot run tests")
}

/** `testFrameworks:` in bleep.yaml names a `Framework` class for a framework bleep does not know.
  *
  * It was documented for a long time before it did anything: the value reached `ResolvedProject` and the Java API and stopped there, so a user with an
  * unsupported framework followed the documentation and got silence. These two tests are the difference — the same class is discovered with the declaration and
  * not without it, so the setting cannot quietly go inert again.
  */
class DeclaredTestFrameworkTest extends AnyFunSuite with Matchers {
  private val project = model.CrossProjectName(model.ProjectName("mytest"), None)

  /** This JVM's own classpath, which carries [[MarkerFramework]] the way a project's classpath would carry its framework. */
  private def currentClasspath: List[Path] =
    System.getProperty("java.class.path").split(File.pathSeparatorChar).toList.filter(_.nonEmpty).map(Paths.get(_))

  /** A directory shaped like a project's compiled output, holding exactly one class. */
  private def classesDirContaining(cls: Class[?]): Path = {
    val dir = Files.createTempDirectory("declared-framework-test")
    val relPath = cls.getName.replace('.', '/') + ".class"
    val source = Option(getClass.getClassLoader.getResourceAsStream(relPath))
      .getOrElse(fail(s"$relPath is not on this JVM's classpath"))
    val target = dir.resolve(relPath)
    Files.createDirectories(target.getParent)
    try Files.copy(source, target)
    finally source.close()
    dir
  }

  test("a framework named in testFrameworks: is used, though bleep has never heard of it") {
    val suites = ClasspathTestDiscovery.discover(
      project,
      classesDirContaining(classOf[MarkerSuite]),
      currentClasspath,
      declaredFrameworks = List(classOf[MarkerFramework].getName)
    )

    suites.map(_.className) shouldBe List(classOf[MarkerSuite].getName)
    // Carries the declared class, not the display name — that is what the fork instantiates.
    suites.head.selection shouldBe FrameworkSelection.SbtTestInterface("marker", classOf[MarkerFramework].getName)
  }

  test("a declared framework that is not on the classpath is an error, not a shrug") {
    val thrown = intercept[RuntimeException] {
      ClasspathTestDiscovery.discover(
        project,
        classesDirContaining(classOf[MarkerSuite]),
        currentClasspath,
        declaredFrameworks = List("com.novocode.junit.JUnitFramework")
      )
    }
    thrown.getMessage should include("mytest")
    thrown.getMessage should include("com.novocode.junit.JUnitFramework")
    // The message has to name the way out, because the most likely source of a stale entry is bleep's own importer.
    thrown.getMessage should include("import-maven")
  }

  test("without the declaration the same class is invisible") {
    val suites = ClasspathTestDiscovery.discover(
      project,
      classesDirContaining(classOf[MarkerSuite]),
      currentClasspath,
      declaredFrameworks = Nil
    )
    withClue("nothing built in should match a class whose only marker is a base class bleep does not know: ")(suites shouldBe empty)
  }
}
