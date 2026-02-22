package bleep.analysis

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

class EcjCompilationProgressBridgeTest extends AnyFunSuite with Matchers {

  /** Resolve ECJ jar and create classloader, same as ZincBridge does at runtime */
  private def withEcjClassLoader(ecjVersion: String)(f: ClassLoader => Unit): Unit = {
    val dep = bleep.model.Dep.Java("org.eclipse.jdt", "ecj", ecjVersion)
    val combo = bleep.model.VersionCombo.Java
    val ecjJars = dep.asJava(combo) match {
      case Right(javaDep) =>
        coursier.Fetch().addDependencies(javaDep.dependency).run().map(_.toPath)
      case Left(e) =>
        throw new RuntimeException(s"Failed to resolve ECJ $ecjVersion: $e")
    }
    val ecjClassLoader = new java.net.URLClassLoader(
      ecjJars.map(_.toUri.toURL).toArray,
      getClass.getClassLoader
    )
    try f(ecjClassLoader)
    finally ecjClassLoader.close()
  }

  test("generateBridgeClass produces valid bytecode that loads in ECJ classloader") {
    withEcjClassLoader("3.40.0") { ecjClassLoader =>
      val bytes = EcjCompiler.generateBridgeClass()
      bytes.length should be > 0

      // Define class in a child classloader of ECJ's classloader
      val bridgeClassLoader = new ClassLoader(ecjClassLoader) {
        override def findClass(name: String): Class[?] =
          if (name == "bleep.analysis.EcjCompilationProgressBridge")
            defineClass(name, bytes, 0, bytes.length)
          else throw new ClassNotFoundException(name)
      }

      val bridgeClass = bridgeClassLoader.loadClass("bleep.analysis.EcjCompilationProgressBridge")
      val progressClass = ecjClassLoader.loadClass("org.eclipse.jdt.core.compiler.CompilationProgress")

      // Bridge must be a subclass of CompilationProgress
      progressClass.isAssignableFrom(bridgeClass) shouldBe true

      // Must have the 3-arg constructor
      val ctor = bridgeClass.getConstructor(classOf[AtomicInteger], classOf[AtomicInteger], classOf[AtomicBoolean])
      ctor should not be null
    }
  }

  test("bridge worked() updates AtomicIntegers correctly") {
    withEcjClassLoader("3.40.0") { ecjClassLoader =>
      val bytes = EcjCompiler.generateBridgeClass()
      val bridgeClassLoader = new ClassLoader(ecjClassLoader) {
        override def findClass(name: String): Class[?] =
          if (name == "bleep.analysis.EcjCompilationProgressBridge")
            defineClass(name, bytes, 0, bytes.length)
          else throw new ClassNotFoundException(name)
      }
      val bridgeClass = bridgeClassLoader.loadClass("bleep.analysis.EcjCompilationProgressBridge")

      val workedSoFar = new AtomicInteger(0)
      val totalWork = new AtomicInteger(0)
      val cancelFlag = new AtomicBoolean(false)

      val instance = bridgeClass
        .getConstructor(classOf[AtomicInteger], classOf[AtomicInteger], classOf[AtomicBoolean])
        .newInstance(workedSoFar, totalWork, cancelFlag)

      val beginMethod = bridgeClass.getMethod("begin", classOf[Int])
      val workedMethod = bridgeClass.getMethod("worked", classOf[Int], classOf[Int])

      // begin(100) sets total
      beginMethod.invoke(instance, Integer.valueOf(100))
      totalWork.get() shouldBe 100

      // worked(10, 90) -> workedSoFar=10, totalWork=10+90=100
      workedMethod.invoke(instance, Integer.valueOf(10), Integer.valueOf(90))
      workedSoFar.get() shouldBe 10
      totalWork.get() shouldBe 100

      // worked(20, 70) -> workedSoFar=30, totalWork=30+70=100
      workedMethod.invoke(instance, Integer.valueOf(20), Integer.valueOf(70))
      workedSoFar.get() shouldBe 30
      totalWork.get() shouldBe 100

      // worked(70, 0) -> workedSoFar=100, totalWork=100+0=100
      workedMethod.invoke(instance, Integer.valueOf(70), Integer.valueOf(0))
      workedSoFar.get() shouldBe 100
      totalWork.get() shouldBe 100
    }
  }

  test("bridge isCancelled() reads from AtomicBoolean") {
    withEcjClassLoader("3.40.0") { ecjClassLoader =>
      val bytes = EcjCompiler.generateBridgeClass()
      val bridgeClassLoader = new ClassLoader(ecjClassLoader) {
        override def findClass(name: String): Class[?] =
          if (name == "bleep.analysis.EcjCompilationProgressBridge")
            defineClass(name, bytes, 0, bytes.length)
          else throw new ClassNotFoundException(name)
      }
      val bridgeClass = bridgeClassLoader.loadClass("bleep.analysis.EcjCompilationProgressBridge")

      val workedSoFar = new AtomicInteger(0)
      val totalWork = new AtomicInteger(0)
      val cancelFlag = new AtomicBoolean(false)

      val instance = bridgeClass
        .getConstructor(classOf[AtomicInteger], classOf[AtomicInteger], classOf[AtomicBoolean])
        .newInstance(workedSoFar, totalWork, cancelFlag)

      val isCancelledMethod = bridgeClass.getMethod("isCancelled")

      isCancelledMethod.invoke(instance).asInstanceOf[java.lang.Boolean].booleanValue() shouldBe false

      cancelFlag.set(true)

      isCancelledMethod.invoke(instance).asInstanceOf[java.lang.Boolean].booleanValue() shouldBe true
    }
  }

  test("createMainWithProgress creates Main with progress bridge") {
    withEcjClassLoader("3.40.0") { ecjClassLoader =>
      val mainClass = ecjClassLoader.loadClass("org.eclipse.jdt.internal.compiler.batch.Main")

      val workedSoFar = new AtomicInteger(0)
      val totalWork = new AtomicInteger(0)
      val cancelFlag = new AtomicBoolean(false)

      val outPrint = new java.io.PrintWriter(new java.io.ByteArrayOutputStream())
      val errPrint = new java.io.PrintWriter(new java.io.ByteArrayOutputStream())

      val (ecjMain, hasProgress) = EcjCompiler.createMainWithProgress(
        mainClass,
        ecjClassLoader,
        outPrint,
        errPrint,
        workedSoFar,
        totalWork,
        cancelFlag
      )

      hasProgress shouldBe true
      ecjMain should not be null
      mainClass.isInstance(ecjMain) shouldBe true
    }
  }

  test("createMainWithProgress falls back when CompilationProgress is missing") {
    // Use a classloader that deliberately hides CompilationProgress
    val fakeClassLoader = new ClassLoader(getClass.getClassLoader) {
      override def loadClass(name: String, resolve: Boolean): Class[?] =
        if (name == "org.eclipse.jdt.core.compiler.CompilationProgress")
          throw new ClassNotFoundException(name)
        else
          super.loadClass(name, resolve)
    }

    // We can't load Main either (it's in ECJ), but createMainWithProgress
    // should catch ClassNotFoundException before reaching that point.
    // Test with real ECJ to verify the fallback path explicitly.
    withEcjClassLoader("3.40.0") { ecjClassLoader =>
      val mainClass = ecjClassLoader.loadClass("org.eclipse.jdt.internal.compiler.batch.Main")

      // Create a wrapper classloader that hides CompilationProgress
      val hidingClassLoader = new ClassLoader(ecjClassLoader) {
        override def loadClass(name: String, resolve: Boolean): Class[?] =
          if (name == "org.eclipse.jdt.core.compiler.CompilationProgress")
            throw new ClassNotFoundException(name)
          else
            super.loadClass(name, resolve)
      }

      val workedSoFar = new AtomicInteger(0)
      val totalWork = new AtomicInteger(0)
      val cancelFlag = new AtomicBoolean(false)

      val outPrint = new java.io.PrintWriter(new java.io.ByteArrayOutputStream())
      val errPrint = new java.io.PrintWriter(new java.io.ByteArrayOutputStream())

      val (ecjMain, hasProgress) = EcjCompiler.createMainWithProgress(
        mainClass,
        hidingClassLoader,
        outPrint,
        errPrint,
        workedSoFar,
        totalWork,
        cancelFlag
      )

      hasProgress shouldBe false
      ecjMain should not be null
    }
  }
}
