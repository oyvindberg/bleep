package bleep.bsp

import bleep.model.CrossProjectName
import sbt.testing._

import java.io.File
import java.lang.reflect.Modifier
import java.net.URLClassLoader
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._
import scala.util.Try

/** Discovered test suite ready for execution */
case class DiscoveredTestSuite(
    project: CrossProjectName,
    className: String,
    framework: String
)

/** Discovers test suites by scanning compiled class files.
  *
  * Supports multiple discovery mechanisms:
  *   1. sbt-testing Framework fingerprints (ScalaTest, munit, utest, ZIO Test, specs2, etc.)
  *   2. Direct annotation scanning (JUnit 4/5, TestNG, kotlin.test)
  *   3. Base class detection (Kotest, Spock)
  *   4. Naming convention fallback (Maven/Gradle patterns)
  */
object ClasspathTestDiscovery {

  // ============================================================================
  // Known sbt-testing Framework implementations
  // ============================================================================
  private val knownFrameworks: List[String] = List(
    // Scala test frameworks
    "org.scalatest.tools.Framework",
    "munit.Framework",
    "utest.runner.Framework",
    "zio.test.sbt.ZTestFramework",
    "org.specs2.runner.Specs2Framework",
    "weaver.sbt.WeaverFramework",
    "org.scalacheck.ScalaCheckFramework",
    "hedgehog.sbt.Framework",
    "minitest.runner.Framework",
    // JUnit frameworks (sbt-testing adapters)
    "com.github.sbt.junit.jupiter.api.JupiterFramework", // JUnit 5 (current, 0.11.3+)
    "com.github.sbt.junit.JupiterFramework", // JUnit 5 (older version)
    "net.aichler.jupiter.api.JupiterFramework", // JUnit 5 (legacy)
    "com.novocode.junit.JUnitFramework", // JUnit 4
    // TestNG (via mill adapter - different package names across versions)
    "mill.testng.TestNGFramework",
    "mill.contrib.testng.TestNGFramework"
  )

  // ============================================================================
  // Test annotations for direct scanning (when no sbt-testing framework found)
  // ============================================================================
  private val testAnnotations: List[String] = List(
    // JUnit 5 (Jupiter)
    "org.junit.jupiter.api.Test",
    "org.junit.jupiter.api.RepeatedTest",
    "org.junit.jupiter.api.ParameterizedTest",
    "org.junit.jupiter.api.TestFactory",
    "org.junit.jupiter.api.TestTemplate",
    // JUnit 4
    "org.junit.Test",
    // TestNG
    "org.testng.annotations.Test",
    // Kotlin test
    "kotlin.test.Test"
  )

  // ============================================================================
  // Base classes for framework detection (fallback when no sbt-testing Framework found)
  // These match the actual fingerprints used by each framework
  // ============================================================================
  private val testBaseClasses: Map[String, List[String]] = Map(
    // Kotest (Kotlin) - uses JUnit Platform, check Spec hierarchy
    "Kotest" -> List(
      "io.kotest.core.spec.Spec",
      "io.kotest.core.spec.style.FunSpec",
      "io.kotest.core.spec.style.StringSpec",
      "io.kotest.core.spec.style.BehaviorSpec",
      "io.kotest.core.spec.style.DescribeSpec",
      "io.kotest.core.spec.style.ShouldSpec",
      "io.kotest.core.spec.style.FeatureSpec",
      "io.kotest.core.spec.style.ExpectSpec",
      "io.kotest.core.spec.style.FreeSpec",
      "io.kotest.core.spec.style.WordSpec",
      "io.kotest.core.spec.style.AnnotationSpec"
    ),
    // Spock (Groovy) - uses JUnit Platform
    "Spock" -> List(
      "spock.lang.Specification"
    ),
    // ScalaTest - fingerprint: SubclassFingerprint("org.scalatest.Suite")
    "ScalaTest" -> List(
      "org.scalatest.Suite"
    ),
    // ZIO Test - fingerprint: SubclassFingerprint("zio.test.ZIOSpecAbstract", isModule=true)
    "ZIO Test" -> List(
      "zio.test.ZIOSpecAbstract",
      "zio.test.ZIOSpecDefault",
      "zio.test.DefaultRunnableSpec"
    ),
    // munit - fingerprint: SubclassFingerprint("munit.Suite")
    "MUnit" -> List(
      "munit.Suite",
      "munit.FunSuite"
    ),
    // utest - fingerprint: SubclassFingerprint("utest.TestSuite", isModule=true/false)
    "uTest" -> List(
      "utest.TestSuite"
    ),
    // specs2 - fingerprint: SubclassFingerprint("org.specs2.specification.core.SpecificationStructure")
    "specs2" -> List(
      "org.specs2.specification.core.SpecificationStructure",
      "org.specs2.Specification",
      "org.specs2.mutable.Specification"
    ),
    // Weaver - fingerprint: SubclassFingerprint("weaver.BaseSuiteClass", isModule=true)
    "Weaver" -> List(
      "weaver.BaseSuiteClass",
      "weaver.SimpleIOSuite",
      "weaver.IOSuite",
      "weaver.MutableIOSuite"
    ),
    // ScalaCheck - fingerprint: SubclassFingerprint("org.scalacheck.Properties")
    "ScalaCheck" -> List(
      "org.scalacheck.Properties"
    ),
    // JUnit 3 - fingerprint: SubclassFingerprint("junit.framework.TestCase")
    "JUnit" -> List(
      "junit.framework.TestCase"
    )
  )

  // ============================================================================
  // Naming patterns for fallback discovery (Maven/Gradle conventions)
  // ============================================================================
  private val testNamePatterns: List[String] = List(
    "^Test[A-Z].*", // Test* (Maven default)
    ".*Test$", // *Test (Maven default)
    ".*Tests$", // *Tests (Maven default)
    ".*TestCase$", // *TestCase (Maven default)
    ".*Spec$", // *Spec (ScalaTest, Spock convention)
    ".*Specification$", // *Specification (Spock convention)
    ".*Suite$", // *Suite (ScalaTest convention)
    ".*IT$", // Integration tests
    ".*IntegrationTest$"
  )

  // ============================================================================
  // Main discovery entry point
  // ============================================================================

  /** Discover test suites in a project's compiled classes.
    *
    * Uses multiple strategies in order:
    *   1. sbt-testing Framework fingerprints
    *   2. Direct annotation scanning
    *   3. Base class detection
    *   4. Naming convention fallback
    *
    * @param project
    *   the project name
    * @param classesDir
    *   directory containing compiled .class files
    * @param classpath
    *   full classpath including dependencies
    * @return
    *   list of discovered test suites
    */
  def discover(
      project: CrossProjectName,
      classesDir: Path,
      classpath: List[Path]
  ): List[DiscoveredTestSuite] = {
    if (!Files.isDirectory(classesDir)) {
      return Nil
    }

    // Create classloader from full classpath
    val urls = (classesDir :: classpath).map(_.toUri.toURL).toArray
    val classLoader = new URLClassLoader(urls, getClass.getClassLoader)

    try {
      val classFiles = collectClassFiles(classesDir)
      val classNames = classFiles.map(f => classFileToClassName(classesDir, f))

      // Strategy 1: sbt-testing Framework fingerprints
      val frameworkDiscovered = discoverViaFrameworks(project, classNames, classLoader)

      // Get classes not yet discovered
      val discoveredClassNames = frameworkDiscovered.map(_.className).toSet
      val remainingClasses = classNames.filterNot(discoveredClassNames.contains)

      // Strategy 2: Direct annotation scanning (JUnit 4/5, TestNG, kotlin.test)
      val annotationDiscovered = discoverViaAnnotations(project, remainingClasses, classLoader)

      // Get classes still not discovered
      val annotationDiscoveredNames = annotationDiscovered.map(_.className).toSet
      val stillRemaining = remainingClasses.filterNot(annotationDiscoveredNames.contains)

      // Strategy 3: Base class detection (Kotest, Spock)
      val baseClassDiscovered = discoverViaBaseClasses(project, stillRemaining, classLoader)

      // Combine all discovered tests and deduplicate
      val allDiscovered = frameworkDiscovered ++ annotationDiscovered ++ baseClassDiscovered

      // Deduplicate: when we have both X and X$ for the same framework, keep only X
      // (X is the module class, X$ is the object - specs2 expects X for objects)
      val deduped = allDiscovered
        .groupBy(s => (s.project, s.className.stripSuffix("$"), s.framework))
        .map { case (_, suites) =>
          // Prefer the version without $ suffix
          suites.find(!_.className.endsWith("$")).getOrElse(suites.head)
        }
        .toList

      deduped
    } finally classLoader.close()
  }

  /** Discover tests using optional naming convention fallback. Only use this when no tests found via other methods.
    */
  def discoverWithFallback(
      project: CrossProjectName,
      classesDir: Path,
      classpath: List[Path]
  ): List[DiscoveredTestSuite] = {
    val discovered = discover(project, classesDir, classpath)
    if (discovered.nonEmpty) {
      discovered
    } else {
      // Fallback: try naming conventions
      discoverByNamingConvention(project, classesDir)
    }
  }

  // ============================================================================
  // Strategy 1: sbt-testing Framework fingerprints
  // ============================================================================

  private def discoverViaFrameworks(
      project: CrossProjectName,
      classNames: List[String],
      classLoader: ClassLoader
  ): List[DiscoveredTestSuite] = {
    val frameworks = loadFrameworks(classLoader)

    if (frameworks.isEmpty) {
      return Nil
    }

    // Get fingerprints from all frameworks
    val fingerprintsByFramework: List[(Framework, Fingerprint)] = frameworks.flatMap { fw =>
      fw.fingerprints().toList.map(fp => (fw, fp))
    }

    classNames.flatMap { className =>
      matchFingerprint(className, classLoader, fingerprintsByFramework).map { case (fw, _) =>
        DiscoveredTestSuite(project, className, fw.name())
      }
    }
  }

  /** Load all available test frameworks from the classpath */
  private def loadFrameworks(classLoader: ClassLoader): List[Framework] =
    knownFrameworks.flatMap { fqn =>
      Try {
        val cls = classLoader.loadClass(fqn)
        cls.getDeclaredConstructor().newInstance().asInstanceOf[Framework]
      }.toOption
    }

  /** Try to match a class against fingerprints */
  private def matchFingerprint(
      className: String,
      classLoader: ClassLoader,
      fingerprints: List[(Framework, Fingerprint)]
  ): Option[(Framework, Fingerprint)] = {
    val clazz = Try(classLoader.loadClass(className)).toOption

    clazz.flatMap { cls =>
      // Skip abstract classes and interfaces
      if (Modifier.isAbstract(cls.getModifiers) || cls.isInterface) {
        return None
      }

      fingerprints.find { case (_, fp) =>
        fp match {
          case sfp: SubclassFingerprint =>
            val hasConstructor = !sfp.requireNoArgConstructor || hasNoArgConstructor(cls)
            hasConstructor && Try {
              val superclass = classLoader.loadClass(sfp.superclassName())
              if (sfp.isModule) {
                // Check if it's a Scala object
                val moduleName = className + "$"
                Try(classLoader.loadClass(moduleName))
                  .map(m => superclass.isAssignableFrom(m))
                  .getOrElse(false)
              } else {
                superclass.isAssignableFrom(cls)
              }
            }.getOrElse(false)

          case afp: AnnotatedFingerprint =>
            val annotationClass = Try(classLoader.loadClass(afp.annotationName())).toOption
            annotationClass.exists { annClass =>
              if (afp.isModule) {
                val moduleName = className + "$"
                Try(classLoader.loadClass(moduleName))
                  .map(_.getAnnotations.exists(a => annClass.isAssignableFrom(a.annotationType())))
                  .getOrElse(false)
              } else {
                cls.getAnnotations.exists(a => annClass.isAssignableFrom(a.annotationType()))
              }
            }

          case _ =>
            false
        }
      }
    }
  }

  // ============================================================================
  // Strategy 2: Direct annotation scanning
  // ============================================================================

  private def discoverViaAnnotations(
      project: CrossProjectName,
      classNames: List[String],
      classLoader: ClassLoader
  ): List[DiscoveredTestSuite] = {
    // Check which framework runtimes are actually available
    val availableRuntimes = detectAvailableRuntimes(classLoader)

    classNames.flatMap { className =>
      detectFrameworkByAnnotation(className, classLoader).flatMap { framework =>
        // Only include if we can actually run this framework
        val runtimeAvailable = framework match {
          case "JUnit Jupiter" => availableRuntimes.contains("junit5") || availableRuntimes.contains("junit4")
          case "JUnit"         => availableRuntimes.contains("junit4") || availableRuntimes.contains("junit5")
          case "TestNG"        => availableRuntimes.contains("testng")
          case "kotlin.test"   => availableRuntimes.contains("junit5") || availableRuntimes.contains("junit4")
          case _               => true // Unknown framework, let it try
        }
        if (runtimeAvailable) Some(DiscoveredTestSuite(project, className, framework))
        else None
      }
    }
  }

  /** Check which test framework runtimes are available on the classpath */
  private def detectAvailableRuntimes(classLoader: ClassLoader): Set[String] = {
    val runtimes = scala.collection.mutable.Set[String]()

    // JUnit 5 (Jupiter) runtimes
    val junit5Classes = List(
      "com.github.sbt.junit.jupiter.api.JupiterFramework",
      "com.github.sbt.junit.JupiterFramework",
      "net.aichler.jupiter.api.JupiterFramework"
    )
    if (junit5Classes.exists(c => Try(classLoader.loadClass(c)).isSuccess)) {
      runtimes += "junit5"
    }

    // JUnit 4 runtime
    if (Try(classLoader.loadClass("com.novocode.junit.JUnitFramework")).isSuccess) {
      runtimes += "junit4"
    }

    // TestNG runtime (try both Mill package names)
    val testngClasses = List(
      "mill.testng.TestNGFramework",
      "mill.contrib.testng.TestNGFramework"
    )
    if (testngClasses.exists(c => Try(classLoader.loadClass(c)).isSuccess)) {
      runtimes += "testng"
    }

    runtimes.toSet
  }

  /** Detect test framework by scanning for test annotations */
  private def detectFrameworkByAnnotation(
      className: String,
      classLoader: ClassLoader
  ): Option[String] =
    Try(classLoader.loadClass(className)).toOption.flatMap { cls =>
      // Skip abstract classes and interfaces
      if (Modifier.isAbstract(cls.getModifiers) || cls.isInterface) {
        return None
      }

      // Check for class-level @Test annotation (TestNG style)
      val classLevelAnnotation = findTestAnnotation(cls.getAnnotations, classLoader)

      // Check for method-level test annotations
      val methodLevelAnnotation = cls.getDeclaredMethods.flatMap { method =>
        findTestAnnotation(method.getAnnotations, classLoader)
      }.headOption

      // Determine framework from annotation
      (classLevelAnnotation orElse methodLevelAnnotation).map {
        case ann if ann.contains("jupiter") => "JUnit Jupiter"
        case ann if ann.contains("junit")   => "JUnit"
        case ann if ann.contains("testng")  => "TestNG"
        case ann if ann.contains("kotlin")  => "kotlin.test"
        case _                              => "JUnit" // Default
      }
    }

  /** Find which test annotation is present */
  private def findTestAnnotation(
      annotations: Array[java.lang.annotation.Annotation],
      classLoader: ClassLoader
  ): Option[String] =
    testAnnotations.find { annName =>
      Try(classLoader.loadClass(annName)).toOption.exists { annClass =>
        annotations.exists(a => annClass.isAssignableFrom(a.annotationType()))
      }
    }

  // ============================================================================
  // Strategy 3: Base class detection (Kotest, Spock, etc.)
  // ============================================================================

  private def discoverViaBaseClasses(
      project: CrossProjectName,
      classNames: List[String],
      classLoader: ClassLoader
  ): List[DiscoveredTestSuite] = {
    // For base class detection, the base class itself being loadable means
    // the framework is on classpath. But we still need to verify the runtime
    // (sbt-testing Framework) is available to actually run the tests.
    val frameworkRuntimes = Map(
      "Kotest" -> List("io.kotest.runner.junit.platform.KotestJunitPlatformTestEngine"),
      "Spock" -> List("org.spockframework.runtime.SpockEngine"),
      "ScalaTest" -> List("org.scalatest.tools.Framework"),
      "ZIO Test" -> List("zio.test.sbt.ZTestFramework"),
      "MUnit" -> List("munit.Framework"),
      "uTest" -> List("utest.runner.Framework"),
      "specs2" -> List("org.specs2.runner.Specs2Framework"),
      "Weaver" -> List("weaver.sbt.WeaverFramework"),
      "ScalaCheck" -> List("org.scalacheck.ScalaCheckFramework"),
      "JUnit" -> List("com.novocode.junit.JUnitFramework", "com.github.sbt.junit.jupiter.api.JupiterFramework", "com.github.sbt.junit.JupiterFramework")
    )

    // Pre-check which frameworks have their runtime available
    val availableFrameworks = frameworkRuntimes.collect {
      case (framework, runtimeClasses) if runtimeClasses.exists(c => Try(classLoader.loadClass(c)).isSuccess) =>
        framework
    }.toSet

    classNames.flatMap { className =>
      detectFrameworkByBaseClass(className, classLoader).flatMap { framework =>
        // Kotest and Spock use JUnit Platform, so they might work without explicit runtime
        // if JUnit 5 is available
        val canRun = availableFrameworks.contains(framework) ||
          (framework == "Kotest" && availableFrameworks.exists(f => f.contains("JUnit"))) ||
          (framework == "Spock" && availableFrameworks.exists(f => f.contains("JUnit")))

        if (canRun) Some(DiscoveredTestSuite(project, className, framework))
        else None
      }
    }
  }

  /** Detect test framework by checking base class inheritance */
  private def detectFrameworkByBaseClass(
      className: String,
      classLoader: ClassLoader
  ): Option[String] =
    Try(classLoader.loadClass(className)).toOption.flatMap { cls =>
      // Skip abstract classes and interfaces
      if (Modifier.isAbstract(cls.getModifiers) || cls.isInterface) {
        return None
      }

      // Also check module class for Scala objects
      val classesToCheck = List(
        Some(cls),
        Try(classLoader.loadClass(className + "$")).toOption
      ).flatten

      testBaseClasses.collectFirst {
        case (framework, baseClasses) if baseClasses.exists { baseName =>
              Try(classLoader.loadClass(baseName)).toOption.exists { baseClass =>
                classesToCheck.exists(baseClass.isAssignableFrom)
              }
            } =>
          framework
      }
    }

  // ============================================================================
  // Strategy 4: Naming convention fallback (Maven/Gradle patterns)
  // ============================================================================

  /** Discover tests by naming convention (use as last resort) */
  private def discoverByNamingConvention(
      project: CrossProjectName,
      classesDir: Path
  ): List[DiscoveredTestSuite] = {
    if (!Files.isDirectory(classesDir)) return Nil

    val classFiles = collectClassFiles(classesDir)
    val compiledPatterns = testNamePatterns.map(_.r)

    classFiles.flatMap { classFile =>
      val className = classFileToClassName(classesDir, classFile)
      val simpleName = className.split('.').lastOption.getOrElse(className)

      if (compiledPatterns.exists(_.findFirstIn(simpleName).isDefined)) {
        Some(DiscoveredTestSuite(project, className, "unknown"))
      } else {
        None
      }
    }
  }

  // ============================================================================
  // Utility methods
  // ============================================================================

  /** Collect all .class files in a directory (recursively) */
  private def collectClassFiles(dir: Path): List[Path] = {
    if (!Files.isDirectory(dir)) return Nil

    import scala.util.Using
    // Use Using to ensure Files.walk stream is properly closed
    val allClassFiles = Using(Files.walk(dir)) { stream =>
      stream
        .iterator()
        .asScala
        .filter(p => p.toString.endsWith(".class"))
        .filter { p =>
          val name = p.getFileName.toString
          // Skip: MyTest$Inner.class (inner class - has $ followed by non-empty name)
          // Skip: MyTest$$anon$1.class (anonymous class - has $$)
          !name.contains("$$") && !name.matches(".*\\$[^.]+\\.class")
        }
        .toList
    }.getOrElse(Nil)

    // Get set of non-$ class names
    val nonDollarNames = allClassFiles
      .map(_.getFileName.toString)
      .filter(!_.endsWith("$.class"))
      .map(_.stripSuffix(".class"))
      .toSet

    // Filter out $.class files when the corresponding non-$ class exists
    // This handles Scala objects where we only want to discover the module accessor class,
    // not both the accessor and the object class.
    allClassFiles.filter { p =>
      val name = p.getFileName.toString
      if (name.endsWith("$.class")) {
        val baseName = name.stripSuffix("$.class")
        !nonDollarNames.contains(baseName)
      } else {
        true
      }
    }
  }

  /** Convert class file path to class name */
  private def classFileToClassName(baseDir: Path, classFile: Path): String = {
    val relativePath = baseDir.relativize(classFile).toString
    relativePath
      .stripSuffix(".class")
      .replace(File.separatorChar, '.')
  }

  /** Check if a class has a no-arg constructor */
  private def hasNoArgConstructor(cls: Class[?]): Boolean =
    Try(cls.getDeclaredConstructor()).isSuccess

  // ============================================================================
  // Classpath detection
  // ============================================================================

  /** Quick check if a project might have tests based on classpath. Checks for presence of test framework jars.
    */
  def mightHaveTests(classpath: List[Path]): Boolean = {
    val testFrameworkPatterns = List(
      // Scala test frameworks
      "scalatest",
      "munit",
      "utest",
      "zio-test",
      "specs2",
      "weaver",
      "scalacheck",
      "hedgehog",
      "minitest",
      // Java test frameworks
      "junit",
      "jupiter",
      "junit-platform",
      "testng",
      // Kotlin test frameworks
      "kotest",
      "kotlin-test",
      // Groovy test frameworks
      "spock"
    )

    classpath.exists { p =>
      val name = p.getFileName.toString.toLowerCase
      testFrameworkPatterns.exists(name.contains)
    }
  }

  /** Detect which test frameworks are available on the classpath */
  def detectFrameworks(classpath: List[Path]): List[String] = {
    val frameworkIndicators = Map(
      "scalatest" -> "ScalaTest",
      "munit" -> "MUnit",
      "utest" -> "uTest",
      "zio-test" -> "ZIO Test",
      "specs2" -> "specs2",
      "weaver" -> "Weaver",
      "scalacheck" -> "ScalaCheck",
      "hedgehog" -> "Hedgehog",
      "minitest" -> "minitest",
      "junit-jupiter" -> "JUnit Jupiter",
      "junit-vintage" -> "JUnit Vintage",
      "junit-platform" -> "JUnit Platform",
      "junit4" -> "JUnit 4",
      "testng" -> "TestNG",
      "kotest" -> "Kotest",
      "kotlin-test" -> "kotlin.test",
      "spock" -> "Spock"
    )

    frameworkIndicators.collect {
      case (pattern, name) if classpath.exists(p => p.getFileName.toString.toLowerCase.contains(pattern)) =>
        name
    }.toList
  }
}
