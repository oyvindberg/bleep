package bleep.javaapi

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** Enforces the design rule for the bleepscript module: no references to bleep-internal Scala types in user-facing Java source. The module is intentionally
  * pure Java with all Scala types wrapped, so users never see `bleep.*` / `scala.*` / `ryddig.*` in their IDE.
  */
class BleepscriptGrepInvariantTest extends AnyFunSuite {
  private val javaSources: Path =
    Paths.get(System.getProperty("user.dir")).resolve("bleepscript/src/main/java")

  private val forbidden: List[String] =
    List("bleep.", "scala.", "ryddig.")

  test("bleepscript Java source files contain no forbidden type references") {
    assert(Files.isDirectory(javaSources), s"expected $javaSources to exist")

    val offenders: List[String] = Using.resource(Files.walk(javaSources)) { stream =>
      stream
        .iterator()
        .asScala
        .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".java"))
        .flatMap { file =>
          val content = Files.readString(file)
          forbidden.iterator.collect {
            case needle if content.contains(needle) =>
              s"${javaSources.relativize(file)}: contains forbidden token '$needle'"
          }
        }
        .toList
    }

    if (offenders.nonEmpty) {
      fail(
        s"Found forbidden Scala/bleep/ryddig type references in bleepscript Java sources:\n" +
          offenders.mkString("  - ", "\n  - ", "")
      )
    }
  }
}
