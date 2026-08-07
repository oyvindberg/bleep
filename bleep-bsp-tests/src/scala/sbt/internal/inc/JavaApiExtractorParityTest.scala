// Lives in `sbt.internal.inc` rather than `bleep.analysis` because `classfile.ClassFile` and
// `classfile.Parser` are `private[sbt]` — the classfile side of the comparison is unreachable
// from outside the sbt package.
package sbt.internal.inc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import sbt.internal.inc.classfile.{ClassFile, Parser}
import sbt.util.Logger
import xsbt.api.HashAPI

import java.net.URLClassLoader
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Guards the replacement of zinc's reflective Java API extraction with a classfile-based one.
  *
  * `AnalyzingJavaCompiler.readAPI` builds each compiled class's `xsbti.api.ClassLike` by loading it reflectively and walking it with [[ClassToAPI]]. That is
  * where the bsp server spends the bulk of its CPU, and it is what forces a classloader over the whole project classpath per compile. [[ClassfileToAPI]] builds
  * the same structure straight from the classfile — but today only as a fallback for classes that fail to load, and with `structure.inherited` left empty.
  *
  * ==The property==
  *
  * NOT "both extractors produce the same hash" — [[ClassfileToAPI]]'s own docs say the output need not match reflection byte-for-byte, and it does not. Member
  * types are erased, generic signatures are folded into a synthetic annotation, and so on. Asserting equality would only assert that nobody improved either
  * side.
  *
  * What actually has to hold is *sensitivity dominance*: for every change to a class's public shape, if the reflective hash moves then the classfile hash must
  * move too. Over-sensitivity is safe — it costs a recompile nobody needed. Under-sensitivity is a stale build: `HashAPI.hashStructure0` hashes both
  * `structure.declared` and `structure.inherited`, so a class whose API hash fails to move leaves its dependents un-invalidated, still linked against a
  * signature that no longer exists.
  *
  * ==Why `inheritedMemberChanged` is the interesting one==
  *
  * `B extends A`, `A` gains a changed method, `B` redeclares nothing. `B`'s own classfile is byte-identical across the change, so a classfile extractor that
  * ignores supertypes cannot see it, while reflection can (`Class.getMethods` returns inherited members). That is the single case that makes promoting
  * [[ClassfileToAPI]] unsafe, and the case any replacement has to answer.
  */
class JavaApiExtractorParityTest extends AnyFunSuite with Matchers {

  /** A mutation: the same compilation unit before and after an edit to its public shape. */
  private case class Mutation(name: String, before: Map[String, String], after: Map[String, String], focus: String)

  private val mutations: List[Mutation] = List(
    Mutation(
      "addPublicMethod",
      Map("A.java" -> "public class A { public int one() { return 1; } }"),
      Map("A.java" -> "public class A { public int one() { return 1; } public int two() { return 2; } }"),
      focus = "A"
    ),
    Mutation(
      "changeParameterType",
      Map("A.java" -> "public class A { public void foo(int x) {} }"),
      Map("A.java" -> "public class A { public void foo(long x) {} }"),
      focus = "A"
    ),
    Mutation(
      "changeReturnType",
      Map("A.java" -> "public class A { public int foo() { return 1; } }"),
      Map("A.java" -> "public class A { public long foo() { return 1L; } }"),
      focus = "A"
    ),
    Mutation(
      "addPublicField",
      Map("A.java" -> "public class A { public int x; }"),
      Map("A.java" -> "public class A { public int x; public int y; }"),
      focus = "A"
    ),
    Mutation(
      "publicMethodBecomesPrivate",
      Map("A.java" -> "public class A { public int foo() { return 1; } }"),
      Map("A.java" -> "public class A { private int foo() { return 1; } }"),
      focus = "A"
    ),
    Mutation(
      "addImplementedInterface",
      Map("I.java" -> "public interface I { void go(); }", "A.java" -> "public class A { public void go() {} }"),
      Map("I.java" -> "public interface I { void go(); }", "A.java" -> "public class A implements I { public void go() {} }"),
      focus = "A"
    ),
    // The one that matters: B's own bytes never change, only its supertype's.
    Mutation(
      "inheritedMemberChanged",
      Map("A.java" -> "public class A { public void foo(int x) {} }", "B.java" -> "public class B extends A { }"),
      Map("A.java" -> "public class A { public void foo(long x) {} }", "B.java" -> "public class B extends A { }"),
      focus = "B"
    ),
    // Two levels up, so the supertype walk has to recurse rather than look at direct parents only.
    Mutation(
      "transitivelyInheritedMemberChanged",
      Map(
        "A.java" -> "public class A { public void foo(int x) {} }",
        "B.java" -> "public class B extends A { }",
        "C.java" -> "public class C extends B { }"
      ),
      Map(
        "A.java" -> "public class A { public void foo(long x) {} }",
        "B.java" -> "public class B extends A { }",
        "C.java" -> "public class C extends B { }"
      ),
      focus = "C"
    ),
    // Interfaces are walked too, not just the superclass chain.
    Mutation(
      "inheritedFromInterfaceChanged",
      Map("I.java" -> "public interface I { void go(int x); }", "A.java" -> "public abstract class A implements I { }"),
      Map("I.java" -> "public interface I { void go(long x); }", "A.java" -> "public abstract class A implements I { }"),
      focus = "A"
    ),
    Mutation(
      "inheritedMemberAdded",
      Map("A.java" -> "public class A { public void foo() {} }", "B.java" -> "public class B extends A { }"),
      Map("A.java" -> "public class A { public void foo() {} public void bar() {} }", "B.java" -> "public class B extends A { }"),
      focus = "B"
    )
  )

  /** javac the given sources into a fresh directory, returning it. */
  private def compile(sources: Map[String, String]): Path = {
    val dir = Files.createTempDirectory("api-parity")
    val srcDir = Files.createDirectories(dir.resolve("src"))
    val out = Files.createDirectories(dir.resolve("classes"))
    val files = sources.map { case (name, body) =>
      val f = srcDir.resolve(name)
      Files.writeString(f, body)
      f.toFile
    }.toList
    val javac = javax.tools.ToolProvider.getSystemJavaCompiler
    val fm = javac.getStandardFileManager(null, null, null)
    val units = fm.getJavaFileObjectsFromFiles(files.asJava)
    val ok = javac.getTask(null, fm, null, List("-d", out.toString).asJava, null, units).call()
    fm.close()
    if (!ok) sys.error(s"javac failed for ${sources.keys.mkString(", ")}")
    out
  }

  private def classFilesIn(dir: Path): List[Path] =
    Files.walk(dir).iterator().asScala.filter(p => Files.isRegularFile(p) && p.toString.endsWith(".class")).toList.sortBy(_.toString)

  private def binaryName(dir: Path, cls: Path): String =
    dir.relativize(cls).toString.stripSuffix(".class").replace('/', '.')

  /** className -> API hash, via reflection (what zinc does today). */
  private def reflectiveHashes(dir: Path): Map[String, Int] = {
    val loader = new URLClassLoader(Array(dir.toUri.toURL), ClassLoader.getPlatformClassLoader)
    try {
      val classes = classFilesIn(dir).map(p => Class.forName(binaryName(dir, p), false, loader))
      val (apis, _, _) = ClassToAPI.process(classes, Logger.Null)
      combine(apis)
    } finally loader.close()
  }

  /** className -> API hash, via classfile parsing (what we want to move to). */
  private def classfileHashes(dir: Path): Map[String, Int] = {
    val named: List[(String, ClassFile)] = classFilesIn(dir).map(p => binaryName(dir, p) -> Parser(p, Logger.Null))
    // Supertypes must be resolvable or `structure.inherited` stays empty and the inherited-member
    // cases cannot pass. Reflection gets this from the classloader; here it comes off the same dir.
    val (apis, _) = ClassfileToAPI.process(named, ClassfileToAPI.resolveWithin(named), Logger.Null)
    combine(apis)
  }

  /** Both extractors emit a class AND a module `ClassLike` under the SAME name; keying a map by name alone silently keeps only the last (the module, which for
    * a Java class is empty) and makes every mutation look invisible. Combine every `ClassLike` sharing a name instead.
    */
  private def combine(apis: Seq[xsbti.api.ClassLike]): Map[String, Int] =
    apis.groupBy(_.name).map { case (n, cls) => n -> cls.map(HashAPI.apply).sorted.hashCode }

  /** Names whose hash moved between the two directories. A name appearing on one side only counts as moved. */
  private def moved(before: Map[String, Int], after: Map[String, Int]): Set[String] =
    (before.keySet ++ after.keySet).filter(n => before.get(n) != after.get(n))

  mutations.foreach { m =>
    test(s"sensitivity dominance: ${m.name}") {
      val beforeDir = compile(m.before)
      val afterDir = compile(m.after)

      val reflMoved = moved(reflectiveHashes(beforeDir), reflectiveHashes(afterDir))
      val cfMoved = moved(classfileHashes(beforeDir), classfileHashes(afterDir))

      // Sanity: the mutation must be visible to reflection at all, or the case proves nothing.
      withClue(s"reflective extractor saw no change for ${m.name}; the mutation or the focus class is wrong. ") {
        reflMoved should contain(m.focus)
      }

      withClue(
        s"classfile extractor missed a change reflection caught for ${m.name}. " +
          s"reflection moved=${reflMoved.toList.sorted}, classfile moved=${cfMoved.toList.sorted}. " +
          "Under-sensitivity here means dependents are never invalidated — a stale build, not a slow one. "
      ) {
        reflMoved.subsetOf(cfMoved) shouldBe true
      }
    }
  }

  test("both extractors are deterministic for identical input") {
    val dir = compile(Map("A.java" -> "public class A { public int foo(String s) { return 1; } }"))
    // Non-emptiness first: two empty maps compare equal, so determinism alone proves nothing.
    withClue(s"class files found: ${classFilesIn(dir)}. ") {
      reflectiveHashes(dir) should not be empty
      classfileHashes(dir) should not be empty
    }
    reflectiveHashes(dir) shouldBe reflectiveHashes(dir)
    classfileHashes(dir) shouldBe classfileHashes(dir)
  }
}
