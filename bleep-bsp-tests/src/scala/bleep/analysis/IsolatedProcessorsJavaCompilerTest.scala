package bleep.analysis

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Files
import javax.tools.{StandardLocation, ToolProvider}
import scala.jdk.CollectionConverters.*

class IsolatedProcessorsJavaCompilerTest extends AnyFunSuite {
  private def fileManager = new IsolatedProcessorsJavaCompiler(ToolProvider.getSystemJavaCompiler).getStandardFileManager(null, null, null)

  test("processors are loaded apart from the server's classes, but see the JDK and javac") {
    val processorPath = Files.createTempDirectory("processor-path")
    val fm = fileManager
    fm.setLocationFromPaths(StandardLocation.ANNOTATION_PROCESSOR_PATH, List(processorPath).asJava)
    val loader = fm.getClassLoader(StandardLocation.ANNOTATION_PROCESSOR_PATH)

    // on the server's class path, where error prone found bleep's guava
    assert(Class.forName("com.google.common.base.Ticker") != null)
    assertThrows[ClassNotFoundException](loader.loadClass("com.google.common.base.Ticker"))
    assertThrows[ClassNotFoundException](loader.loadClass(classOf[ZincBridge.type].getName))
    // what a javac plugin implements, and what a processor implements
    assert(loader.loadClass("com.sun.source.util.Plugin").getModule.getName == "jdk.compiler")
    assert(loader.loadClass("javax.annotation.processing.Processor") != null)
  }

  test("without a processor path, processors come from the class path, also apart from the server's classes") {
    val fm = fileManager
    fm.setLocationFromPaths(StandardLocation.CLASS_PATH, List(Files.createTempDirectory("class-path")).asJava)
    assertThrows[ClassNotFoundException](fm.getClassLoader(StandardLocation.CLASS_PATH).loadClass("com.google.common.base.Ticker"))
  }
}
