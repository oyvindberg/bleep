package bleep.bsp

import bleep.model.CrossProjectName
import bleep.testing.FrameworkSelection
import sbt.testing._

import java.io.File
import java.lang.reflect.Modifier
import java.net.URLClassLoader
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._
import scala.util.Try

/** Discovered test suite ready for execution.
  *
  * `selection` is the execution decision — which runner, and for the sbt path which `Framework` class — made here because this is where the classpath is. The
  * display name remains available as [[framework]] for reporting, BSP's `ScalaTestClassesItem` and metrics; nothing dispatches on it.
  */
case class DiscoveredTestSuite(
    project: CrossProjectName,
    className: String,
    selection: FrameworkSelection
) {
  def framework: String = selection.displayName
}

/** Discovers test suites by scanning compiled class files.
  *
  * Three mechanisms, tried in order, each seeing only what the previous one did not claim:
  *   1. sbt-testing Framework fingerprints (ScalaTest, munit, utest, ZIO Test, specs2, etc.)
  *   2. Direct annotation scanning (JUnit 4/5, TestNG, kotlin.test)
  *   3. Base class detection (Kotest, Spock)
  *
  * There used to be a fourth, matching class names against Maven/Gradle conventions (`*Test`, `*Spec`, ...). It was unreachable — its entry point had no
  * callers, both live ones call [[discover]] directly — and it could not have worked: it reported the framework as the literal string "unknown", which the fork
  * would have handed to `Class.forName`. Guessing from a filename cannot say which runner to use, so it is gone rather than repaired.
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
  // Main discovery entry point
  // ============================================================================

  /** Discover test suites in a project's compiled classes.
    *
    * Three strategies, each seeing only the classes the previous one did not claim:
    *   1. sbt-testing Framework fingerprints
    *   2. Direct annotation scanning
    *   3. Base class detection
    *
    * @param project
    *   the project name
    * @param classesDir
    *   directory containing compiled .class files
    * @param classpath
    *   full classpath including dependencies
    * @param declaredFrameworks
    *   `Framework` class names the project named in `testFrameworks:`. Tried ahead of the built-in list, which is the point of the setting: a framework bleep
    *   has never heard of is discoverable as long as it implements sbt test-interface. Still subject to being on the project's own classpath — declaring a
    *   class that is not there discovers nothing, the same as any other candidate.
    * @return
    *   list of discovered test suites
    */
  def discover(
      project: CrossProjectName,
      classesDir: Path,
      classpath: List[Path],
      declaredFrameworks: List[String]
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
      val frameworkDiscovered = discoverViaFrameworks(project, classNames, classLoader, declaredFrameworks)

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

  // ============================================================================
  // Strategy 1: sbt-testing Framework fingerprints
  // ============================================================================

  private def discoverViaFrameworks(
      project: CrossProjectName,
      classNames: List[String],
      classLoader: URLClassLoader,
      declaredFrameworks: List[String]
  ): List[DiscoveredTestSuite] = {
    val frameworks = loadFrameworks(project, classLoader, declaredFrameworks)

    if (frameworks.isEmpty) {
      return Nil
    }

    // Get fingerprints from all frameworks
    val fingerprintsByFramework: List[(Framework, Fingerprint)] = frameworks.flatMap { fw =>
      fw.fingerprints().toList.map(fp => (fw, fp))
    }

    classNames.flatMap { className =>
      matchFingerprint(className, classLoader, fingerprintsByFramework).map { case (fw, _) =>
        DiscoveredTestSuite(project, className, selectionForFramework(fw))
      }
    }
  }

  /** sbt adapters that bridge junit *down* to `sbt.testing.Framework`. When one of these is what matched, the suite still goes to the Launcher: bleep drives
    * the JUnit Platform itself, and running junit through the bridge would lose the `LauncherSession` lifecycle that Quarkus and Spring Boot hook into.
    */
  private val junitAdapterClasses: Set[String] = Set(
    "com.github.sbt.junit.jupiter.api.JupiterFramework",
    "com.github.sbt.junit.JupiterFramework",
    "net.aichler.jupiter.api.JupiterFramework",
    "com.novocode.junit.JUnitFramework"
  )

  /** The framework instance is in hand here, so its implementation class is exact rather than inferred — the whole point of carrying it instead of a name. */
  private def selectionForFramework(fw: Framework): FrameworkSelection = {
    val cls = fw.getClass.getName
    if (junitAdapterClasses.contains(cls)) FrameworkSelection.JUnitPlatform(fw.name())
    else FrameworkSelection.SbtTestInterface(fw.name(), cls)
  }

  /** Load the test frameworks the *project* brings.
    *
    * Two steps on purpose. [[onProjectClasspath]] decides whether a framework is the project's, since a bare `loadClass` would also find one that only bleep
    * happens to depend on and offer it to every project on the machine — see the note there for how that played out. The load itself then goes through the
    * delegating loader, because a `Framework` implements `sbt.testing.Framework` and that interface has to come from bleep's own classloader for the cast on
    * the next line to succeed.
    */
  private def loadFrameworks(project: CrossProjectName, classLoader: URLClassLoader, declaredFrameworks: List[String]): List[Framework] = {
    // A name the user wrote is held to a higher standard than one bleep guessed. `knownFrameworks` is a list of candidates, most of which are absent from any
    // given project, so absence there means nothing. A `testFrameworks:` entry that is absent means the setting is doing nothing and the user believes it is —
    // exactly the silence this setting spent years in.
    val missing = declaredFrameworks.filterNot(fqn => onProjectClasspath(classLoader, fqn))
    if (missing.nonEmpty)
      throw new RuntimeException(
        s"${project.value}: testFrameworks names ${missing.mkString(", ")}, which is not on this project's classpath. " +
          "Add the dependency that provides it, or remove the entry. " +
          "If it is a junit adapter (net.aichler / com.github.sbt.junit / com.novocode.junit) an older `bleep import-maven` wrote it: delete the line, " +
          "junit needs no adapter and its suites are detected automatically."
      )

    // Declared first, so a project's own choice wins the fingerprint race against a built-in candidate. Instantiation failures throw for the same reason as
    // above — for known candidates they are shrugged off, because a broken framework bleep merely suspected is not the user's problem.
    val declared = declaredFrameworks.map(fqn => instantiateFramework(classLoader, fqn))
    val known = knownFrameworks
      .filterNot(declaredFrameworks.contains)
      .filter(fqn => onProjectClasspath(classLoader, fqn))
      .flatMap(fqn => Try(instantiateFramework(classLoader, fqn)).toOption)
    declared ++ known
  }

  private def instantiateFramework(classLoader: URLClassLoader, fqn: String): Framework =
    classLoader.loadClass(fqn).getDeclaredConstructor().newInstance().asInstanceOf[Framework]

  /** Try to match a class against fingerprints */
  private def matchFingerprint(
      className: String,
      classLoader: ClassLoader,
      fingerprints: List[(Framework, Fingerprint)]
  ): Option[(Framework, Fingerprint)] = {
    val clazz = Try(classLoader.loadClass(className)).toOption

    clazz.flatMap { cls =>
      // Skip abstract classes and interfaces
      if (Modifier.isAbstract(cls.getModifiers) || cls.isInterface) None
      else
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
      // URLClassLoader, not ClassLoader: the runtime probes need to ask what is on *these* URLs, not what the parent can also reach.
      classLoader: URLClassLoader
  ): List[DiscoveredTestSuite] = {
    val junitAvailable = junitRuntimeOnClasspath(classLoader)
    val testngBridge = testngBridgeClasses.find(c => onProjectClasspath(classLoader, c))

    classNames.flatMap { className =>
      detectFrameworkByAnnotation(className, classLoader).flatMap { displayName =>
        selectionForAnnotation(displayName, junitAvailable, testngBridge)
          .map(selection => DiscoveredTestSuite(project, className, selection))
      }
    }
  }

  /** TestNG has no sbt adapter of its own; Mill's bridge is what implements `sbt.testing.Framework` for it, and it has moved package once. */
  private val testngBridgeClasses: List[String] = List(
    "mill.testng.TestNGFramework",
    "mill.contrib.testng.TestNGFramework"
  )

  /** Turn an annotation-derived name into an execution decision, or None when the project cannot run it.
    *
    * Everything junit-shaped goes to the Launcher, `kotlin.test` included — on the JVM it delegates to junit, and it previously fell through to the sbt path
    * where the fork tried `Class.forName("kotlin.test")` and died. TestNG is the one annotation-detected framework that really is an sbt-interface framework,
    * and it is offered only when the bridge that implements that interface is actually present: gating on the *annotation* would have discovered suites whose
    * fork then had nothing to run them with.
    */
  private def selectionForAnnotation(
      displayName: String,
      junitAvailable: Boolean,
      testngBridge: Option[String]
  ): Option[FrameworkSelection] =
    displayName match {
      case "TestNG" => testngBridge.map(cls => FrameworkSelection.SbtTestInterface(displayName, cls))
      case _        => if (junitAvailable) Some(FrameworkSelection.JUnitPlatform(displayName)) else None
    }

  /** Is this class on the classloader's *own* URLs — the project's classpath — rather than anywhere its parent can reach?
    *
    * `findResource` does not delegate to the parent, and that is the entire point. [[discover]] parents its loader on bleep's own so that `sbt.testing.*` comes
    * from one place and a `Framework` loaded out of a project can be cast to it, but that also means a plain `loadClass` probe answers "is this class anywhere
    * on bleep's classpath" — the same answer for every project on the machine.
    *
    * Not hypothetical. These probes used to name sbt adapter classes, and they reported "junit5 available" for every project ever compiled: not because the
    * project had junit, but because `bleep-test-runner` declared `jupiter-interface` and `bleep-bsp` depends on `bleep-test-runner`, so the class sat on the
    * server's own classloader. Dropping that unused dependency from the POM turned junit discovery off everywhere, which is how this was found.
    */
  private def onProjectClasspath(classLoader: URLClassLoader, className: String): Boolean =
    classLoader.findResource(className.replace('.', '/') + ".class") != null

  /** Does the project bring a JUnit Platform runtime of its own, so a junit-shaped suite can actually be run?
    *
    * Deliberately the same signal `MultiWorkspaceBspServer.testRuntimeRules` triggers on, because the two have to agree: that table decides what lands on the
    * fork classpath, and this decides whether a suite is offered to it. Ask different questions and they drift — either a suite is discovered that no runtime
    * will run, or a runnable one is silently dropped.
    *
    * So these name what the *project* must supply, not what bleep adds on top. `junit-platform-launcher` is deliberately absent: bleep injects it, at the
    * project's own platform version, and demanding it up front would reject every project that table exists to serve. Likewise the engines — kotest's, spock's,
    * jupiter's — are found by the Launcher through SPI at run time, so the platform's presence is the question, not any particular engine's.
    */
  private def junitRuntimeOnClasspath(classLoader: URLClassLoader): Boolean = {
    val classes = List(
      // Anything with a TestEngine: `junit-platform-commons` comes with `junit-jupiter-api`, `junit-platform-engine` with any engine.
      "org.junit.platform.commons.JUnitException",
      "org.junit.platform.engine.TestEngine",
      // JUnit 4 and 3 — `junit:junit` carries both namespaces, and bleep supplies the vintage engine that runs them.
      "org.junit.Test",
      "junit.framework.TestCase"
    )
    classes.exists(c => onProjectClasspath(classLoader, c))
  }

  /** Detect test framework by scanning for test annotations */
  private def detectFrameworkByAnnotation(
      className: String,
      classLoader: ClassLoader
  ): Option[String] =
    Try(classLoader.loadClass(className)).toOption.flatMap { cls =>
      // Skip abstract classes and interfaces
      if (Modifier.isAbstract(cls.getModifiers) || cls.isInterface) None
      else {
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
      classLoader: URLClassLoader
  ): List[DiscoveredTestSuite] = {
    val junitAvailable = junitRuntimeOnClasspath(classLoader)

    classNames.flatMap { className =>
      detectFrameworkByBaseClass(className, classLoader).flatMap { displayName =>
        selectionForBaseClass(displayName, junitAvailable, classLoader)
          .map(selection => DiscoveredTestSuite(project, className, selection))
      }
    }
  }

  /** Base-class-detected frameworks that run as a JUnit Platform engine rather than through `sbt.testing.Framework`.
    *
    * The engine class itself is not probed. Kotest and Spock register theirs through the platform's `ServiceLoader` SPI, so the Launcher finds it without being
    * told, and naming the class here would break the moment either project moved it. What must be true is that the project has the platform at all, which is
    * exactly [[junitRuntimeOnClasspath]] and exactly what the injection table triggers on.
    */
  private val baseClassJUnitPlatformFrameworks: Set[String] = Set("Kotest", "Spock", "JUnit")

  /** The `sbt.testing.Framework` implementation behind each base-class-detected framework. Several candidates where a framework has renamed it.
    *
    * This mapping is the reason "Spock", "ScalaCheck" and "uTest" used to die in the fork: the display name went over the wire and was looked up in a
    * *different* table on the other side, which had no entry for them, so it fell through to `Class.forName("Spock")`. The lookup now happens once, here, where
    * the answer is checkable against the classpath.
    */
  private val baseClassSbtFrameworks: Map[String, List[String]] = Map(
    "ScalaTest" -> List("org.scalatest.tools.Framework"),
    "ZIO Test" -> List("zio.test.sbt.ZTestFramework"),
    "MUnit" -> List("munit.Framework"),
    "uTest" -> List("utest.runner.Framework"),
    "specs2" -> List("org.specs2.runner.Specs2Framework"),
    "Weaver" -> List("weaver.sbt.WeaverFramework", "weaver.framework.CatsEffect"),
    "ScalaCheck" -> List("org.scalacheck.ScalaCheckFramework")
  )

  private def selectionForBaseClass(
      displayName: String,
      junitAvailable: Boolean,
      classLoader: URLClassLoader
  ): Option[FrameworkSelection] =
    if (baseClassJUnitPlatformFrameworks.contains(displayName))
      if (junitAvailable) Some(FrameworkSelection.JUnitPlatform(displayName)) else None
    else
      baseClassSbtFrameworks
        .getOrElse(displayName, Nil)
        .find(c => onProjectClasspath(classLoader, c))
        .map(cls => FrameworkSelection.SbtTestInterface(displayName, cls))

  /** Detect test framework by checking base class inheritance */
  private def detectFrameworkByBaseClass(
      className: String,
      classLoader: ClassLoader
  ): Option[String] =
    Try(classLoader.loadClass(className)).toOption.flatMap { cls =>
      // Skip abstract classes and interfaces
      if (Modifier.isAbstract(cls.getModifiers) || cls.isInterface) None
      else {
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
    }

  // ============================================================================
  // Strategy 4: Naming convention fallback (Maven/Gradle patterns)
  // ============================================================================

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
