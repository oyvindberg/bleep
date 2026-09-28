package bleep
package bsp

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}

class JdkCompilerOpensTest extends AnyFunSuite {
  test("every package jdk.compiler names, exported, qualified or its own") {
    val output =
      """jdk.compiler@25.0.3
        |exports com.sun.source.tree
        |exports com.sun.tools.javac
        |requires java.base mandated
        |uses javax.annotation.processing.Processor
        |provides javax.tools.JavaCompiler with com.sun.tools.javac.api.JavacTool
        |qualified exports com.sun.tools.javac.api to jdk.internal.md jdk.jshell jdk.javadoc
        |qualified opens com.sun.tools.javac.code to jdk.jshell
        |contains com.sun.tools.javac.processing
        |""".stripMargin
    assert(
      JdkCompilerOpens.parse(output) == List(
        "com.sun.source.tree",
        "com.sun.tools.javac",
        "com.sun.tools.javac.api",
        "com.sun.tools.javac.code",
        "com.sun.tools.javac.processing"
      )
    )
  }

  test("asks the JDK once, then reads what it said") {
    val cacheDir = Files.createTempDirectory("jdk-compiler-opens")
    val javaBin = Path.of(sys.props("java.home"), "bin", "java")
    val logger = ryddig.Loggers.storing()

    val first = JdkCompilerOpens.packages(javaBin, cacheDir, logger)
    assert(first.contains("com.sun.tools.javac.api"))
    assert(first.contains("com.sun.tools.javac.processing"))

    val cached = Files.list(cacheDir.resolve("jdk-compiler-packages")).toList
    assert(cached.size == 1)
    // what the cache says wins: the JDK is not asked again
    Files.writeString(cached.get(0), "only.this")
    assert(JdkCompilerOpens.packages(javaBin, cacheDir, logger) == List("only.this"))
    assert(JdkCompilerOpens(javaBin, cacheDir, logger) == List("--add-opens", "jdk.compiler/only.this=ALL-UNNAMED"))
  }
}
