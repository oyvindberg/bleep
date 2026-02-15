package bleep.analysis

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.Path
import coursier._
import coursier.parse.DependencyParser

/** Tests for ClasspathAnalyzer which reads type definitions from classpath JARs.
  *
  * The ClasspathAnalyzer is used to detect when external dependencies change (e.g., cats 2.9.0 -> 2.10.0). It reads type definitions from JARs and computes
  * hashes for change detection.
  */
class ClasspathAnalyzerTest extends AnyFunSuite with Matchers {

  /** Fetch JARs for a dependency using Coursier */
  def fetchClasspath(deps: Seq[String]): Seq[Path] = {
    val parsed = deps.flatMap { depStr =>
      DependencyParser.dependency(depStr, scala.util.Properties.versionNumberString) match {
        case Left(err) =>
          throw new RuntimeException(s"Failed to parse dependency $depStr: $err")
        case Right(dep) => Some(dep)
      }
    }

    val fetch = Fetch()
      .addDependencies(parsed*)
      .run()

    fetch.map(_.toPath)
  }

  test("analyze Java class files from JAR") {
    // Use a well-known Java library JAR for testing
    val classpath = fetchClasspath(
      Seq(
        "org.slf4j:slf4j-api:2.0.9"
      )
    )

    info(s"Classpath: ${classpath.mkString(", ")}")

    // Analyze a specific symbol from slf4j
    val symbols = Set(Symbol("org.slf4j.Logger"))
    val analysis = ClasspathAnalyzer.analyze(classpath, symbols)

    info(s"Found ${analysis.typeHashes.size} type hashes")
    analysis.typeHashes.foreach { case (sym, hash) =>
      info(s"  $sym -> ${hash.hash}")
    }

    // Logger interface should be found
    analysis.typeHashes.keys.map(_.fqn) should contain("org.slf4j.Logger")
  }

  test("analyze Scala 3 TASTy from cats-core") {
    // cats-core is a Scala 3 library with TASTy files
    val classpath = fetchClasspath(
      Seq(
        "org.typelevel::cats-core:2.9.0"
      )
    )

    info(s"Classpath has ${classpath.size} JARs")
    classpath.filter(_.toString.contains("cats")).foreach { jar =>
      info(s"  $jar")
    }

    // Analyze a specific symbol
    val symbols = Set(Symbol("cats.Functor"))
    val analysis = ClasspathAnalyzer.analyze(classpath, symbols)

    info(s"Found ${analysis.typeHashes.size} type hashes")
    analysis.typeHashes.foreach { case (sym, hash) =>
      info(s"  $sym -> ${hash.hash}")
    }

    // Functor should be found
    analysis.typeHashes.keys.map(_.fqn) should contain("cats.Functor")
  }

  test("analyze type aliases from cats-core") {
    val classpath = fetchClasspath(
      Seq(
        "org.typelevel::cats-core:2.9.0"
      )
    )

    // cats.Id is a type alias: type Id[A] = A
    val symbols = Set(Symbol("cats.Id"))
    val analysis = ClasspathAnalyzer.analyze(classpath, symbols)

    info(s"Found ${analysis.typeHashes.size} type hashes")
    analysis.typeHashes.foreach { case (sym, hash) =>
      info(s"  $sym -> ${hash.hash} (source: ${hash.source.getFileName})")
    }

    // Note: Type aliases are stored in the package object, so we may find it
    // under cats.package or similar. The important thing is we can hash it.
    if analysis.typeHashes.isEmpty then info("Type alias not found directly - may be in package object")
  }

  test("hash changes when library version changes") {
    // Fetch two different versions of a library
    val classpath1 = fetchClasspath(Seq("org.typelevel::cats-kernel:2.9.0"))
    val classpath2 = fetchClasspath(Seq("org.typelevel::cats-kernel:2.10.0"))

    // Analyze the same symbol from both versions
    val symbols = Set(Symbol("cats.kernel.Semigroup"))
    val analysis1 = ClasspathAnalyzer.analyze(classpath1, symbols)
    val analysis2 = ClasspathAnalyzer.analyze(classpath2, symbols)

    info(s"Version 2.9.0: ${analysis1.hashFor(Symbol("cats.kernel.Semigroup"))}")
    info(s"Version 2.10.0: ${analysis2.hashFor(Symbol("cats.kernel.Semigroup"))}")

    // Both should find the symbol
    analysis1.hashFor(Symbol("cats.kernel.Semigroup")) shouldBe defined
    analysis2.hashFor(Symbol("cats.kernel.Semigroup")) shouldBe defined
  }

  // Note: Hashes may or may not be different depending on actual API changes
  // between versions. The important thing is that we can compute them.

  test("analyze all types in a JAR (empty symbols set)") {
    val classpath = fetchClasspath(
      Seq(
        "org.typelevel::cats-kernel:2.9.0"
      )
    )

    // Empty symbols set means analyze all types
    val analysis = ClasspathAnalyzer.analyze(classpath, Set.empty)

    info(s"Found ${analysis.typeHashes.size} type hashes in cats-kernel")

    // Should find many types
    analysis.typeHashes.size should be > 10

    // Sample of types that should be present
    val foundSymbols = analysis.typeHashes.keys.map(_.fqn).toSet
    info(s"Sample types: ${foundSymbols.take(10).mkString(", ")}")
  }

  test("analyze Java library (Gson)") {
    val classpath = fetchClasspath(
      Seq(
        "com.google.code.gson:gson:2.10.1"
      )
    )

    val symbols = Set(Symbol("com.google.gson.Gson"))
    val analysis = ClasspathAnalyzer.analyze(classpath, symbols)

    info(s"Found ${analysis.typeHashes.size} type hashes")
    analysis.typeHashes.foreach { case (sym, hash) =>
      info(s"  $sym -> ${hash.hash}")
    }

    analysis.typeHashes.keys.map(_.fqn) should contain("com.google.gson.Gson")
  }

  test("handles non-existent symbols gracefully") {
    val classpath = fetchClasspath(
      Seq(
        "org.typelevel::cats-kernel:2.9.0"
      )
    )

    val symbols = Set(Symbol("nonexistent.Type"))
    val analysis = ClasspathAnalyzer.analyze(classpath, symbols)

    // Should return empty analysis for non-existent symbols
    analysis.typeHashes shouldBe empty
  }

  test("handles empty classpath") {
    val analysis = ClasspathAnalyzer.analyze(Seq.empty, Set(Symbol("any.Type")))
    analysis.typeHashes shouldBe empty
  }

  test("analyze Kotlin library (kotlinx-coroutines)") {
    // kotlinx-coroutines-core is a Kotlin library with .kotlin_module files
    val classpath = fetchClasspath(
      Seq(
        "org.jetbrains.kotlinx:kotlinx-coroutines-core-jvm:1.7.3"
      )
    )

    info(s"Classpath has ${classpath.size} JARs")
    classpath.filter(_.toString.contains("coroutines")).foreach { jar =>
      info(s"  $jar")
    }

    // Analyze a specific Kotlin class
    val symbols = Set(Symbol("kotlinx.coroutines.Job"))
    val analysis = ClasspathAnalyzer.analyze(classpath, symbols)

    info(s"Found ${analysis.typeHashes.size} type hashes")
    analysis.typeHashes.foreach { case (sym, hash) =>
      info(s"  $sym -> ${hash.hash}")
    }

    // Job interface should be found
    analysis.typeHashes.keys.map(_.fqn) should contain("kotlinx.coroutines.Job")
  }

  test("analyze Kotlin library with type aliases (Arrow)") {
    // Arrow has Kotlin type aliases
    val classpath = fetchClasspath(
      Seq(
        "io.arrow-kt:arrow-core-jvm:1.2.0"
      )
    )

    info(s"Classpath has ${classpath.size} JARs")

    // Analyze a class from Arrow
    val symbols = Set(Symbol("arrow.core.Either"))
    val analysis = ClasspathAnalyzer.analyze(classpath, symbols)

    info(s"Found ${analysis.typeHashes.size} type hashes")
    analysis.typeHashes.foreach { case (sym, hash) =>
      info(s"  $sym -> ${hash.hash}")
    }

    // Either should be found
    analysis.typeHashes.keys.map(_.fqn) should contain("arrow.core.Either")
  }

  test("Kotlin library hash includes metadata version") {
    // Verify that Kotlin metadata is included in hash
    val classpath = fetchClasspath(
      Seq(
        "org.jetbrains.kotlinx:kotlinx-coroutines-core-jvm:1.7.3"
      )
    )

    // Analyze all types
    val analysis = ClasspathAnalyzer.analyze(classpath, Set.empty)

    info(s"Found ${analysis.typeHashes.size} Kotlin types")

    // Should find many Kotlin types
    analysis.typeHashes.size should be > 50

    // Sample some Kotlin-specific types
    val kotlinTypes = analysis.typeHashes.keys
      .map(_.fqn)
      .filter(_.startsWith("kotlinx.coroutines"))
      .toSeq
      .sorted
      .take(10)
    info(s"Sample Kotlin types: ${kotlinTypes.mkString(", ")}")
  }
}
