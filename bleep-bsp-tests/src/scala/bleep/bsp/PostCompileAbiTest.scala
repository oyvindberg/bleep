package bleep.bsp

import bleep.analysis.TransformedClass
import org.objectweb.asm.{ClassWriter, Opcodes}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

/** The per-class ABI delta is what consumers of a post-compiled project are told: a class must be reported exactly when the transform's effect on its API
  * moves.
  */
class PostCompileAbiTest extends AnyFunSuite with Matchers {

  private case class Member(access: Int, name: String, desc: String)

  /** A class with the given members; every method body is `return`, so a "body change" is modelled by the `bodyMarker` constant it loads. */
  private def classBytes(name: String, methods: List[Member], fields: List[Member] = Nil, bodyMarker: Int = 0, interfaces: List[String] = Nil): Array[Byte] = {
    val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    cw.visit(Opcodes.V17, Opcodes.ACC_PUBLIC, name, null, "java/lang/Object", interfaces.toArray)
    fields.foreach(f => cw.visitField(f.access, f.name, f.desc, null, null).visitEnd())
    methods.foreach { m =>
      val mv = cw.visitMethod(m.access, m.name, m.desc, null, null)
      mv.visitCode()
      mv.visitLdcInsn(Integer.valueOf(bodyMarker))
      mv.visitInsn(Opcodes.POP)
      mv.visitInsn(Opcodes.RETURN)
      mv.visitMaxs(0, 0)
      mv.visitEnd()
    }
    cw.visitEnd()
    cw.toByteArray
  }

  private val hello = Member(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "hello", "()V")
  private val added = Member(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "added", "()V")
  private val secret = Member(Opcodes.ACC_PRIVATE | Opcodes.ACC_STATIC | Opcodes.ACC_SYNTHETIC, "$jacocoInit", "()V")

  private def dir(files: (String, Array[Byte])*): Path = {
    val d = Files.createTempDirectory("abi")
    files.foreach { case (rel, bytes) =>
      val f = d.resolve(rel)
      Files.createDirectories(f.getParent)
      Files.write(f, bytes)
    }
    d
  }

  private def delta(pre: Seq[(String, Array[Byte])], post: Seq[(String, Array[Byte])]): List[TransformedClass] =
    PostCompileAbi.delta(dir(pre*), dir(post*))

  private val lib = "lib/Lib.class"

  test("a transform that copies changes no class") {
    delta(Seq(lib -> classBytes("lib/Lib", List(hello))), Seq(lib -> classBytes("lib/Lib", List(hello)))) shouldBe Nil
  }

  test("private members and method bodies are invisible to consumers: coverage-style instrumentation changes no class") {
    val pre = Seq(lib -> classBytes("lib/Lib", List(hello)))
    val instrumented = Seq(lib -> classBytes("lib/Lib", List(hello, secret), fields = List(Member(Opcodes.ACC_PRIVATE, "$jacocoData", "[Z")), bodyMarker = 42))
    delta(pre, instrumented) shouldBe Nil
  }

  test("resources the transform writes are invisible to a consumer's compiler") {
    val classes = Seq(lib -> classBytes("lib/Lib", List(hello)))
    delta(classes, classes :+ ("listing.txt" -> "anything".getBytes)) shouldBe Nil
  }

  test("each class the transform touches is reported on its own, with its kind and the names a consumer can use") {
    val other = "lib/Other.class" -> classBytes("lib/Other", List(hello))
    val pre = Seq(lib -> classBytes("lib/Lib", List(hello)), other, "lib/Gone.class" -> classBytes("lib/Gone", List(hello)))
    val post = Seq(lib -> classBytes("lib/Lib", List(hello, added)), other, "lib/Extra.class" -> classBytes("lib/Extra", List(hello)))
    val result = delta(pre, post).map(c => (c.binaryName, c.kind)).toMap
    result shouldBe Map(
      "lib.Lib" -> TransformedClass.Kind.Changed,
      "lib.Extra" -> TransformedClass.Kind.Added,
      "lib.Gone" -> TransformedClass.Kind.Removed
    )
    delta(pre, post).find(_.binaryName == "lib.Lib").get.names should contain allOf ("Lib", "hello", "added")
  }

  test("a different contribution to a class is a different hash") {
    val pre = Seq(lib -> classBytes("lib/Lib", List(hello)))
    val addsMethod = delta(pre, Seq(lib -> classBytes("lib/Lib", List(hello, added)))).map(_.hash)
    val addsInterface = delta(pre, Seq(lib -> classBytes("lib/Lib", List(hello), interfaces = List("java/io/Serializable")))).map(_.hash)
    addsMethod should have size 1
    addsInterface should have size 1
    addsMethod should not be addsInterface
  }

  test("an API edit to the source that the transform passes through cancels out: consumers are left to zinc") {
    // before the edit: the transform adds `added`
    val before = delta(Seq(lib -> classBytes("lib/Lib", List(hello))), Seq(lib -> classBytes("lib/Lib", List(hello, added)))).map(_.hash)
    // the source gains a public method; the transform still adds `added` on top of whatever the source has
    val sourceMethod = Member(Opcodes.ACC_PUBLIC, "fromSource", "()V")
    val after =
      delta(Seq(lib -> classBytes("lib/Lib", List(hello, sourceMethod))), Seq(lib -> classBytes("lib/Lib", List(hello, sourceMethod, added)))).map(_.hash)
    after shouldBe before
  }

  test("TASTy the transform changes is API, of the class it belongs to") {
    val pre = Seq(lib -> classBytes("lib/Lib", List(hello)), "lib/Lib.tasty" -> Array[Byte](1, 2, 3))
    delta(pre, Seq(lib -> classBytes("lib/Lib", List(hello)), "lib/Lib.tasty" -> Array[Byte](1, 2, 4))).map(c => (c.binaryName, c.kind)) shouldBe
      List("lib.Lib" -> TransformedClass.Kind.Changed)
  }

  test("the file format reads back what it wrote") {
    val classes = List(
      TransformedClass("lib.Lib", TransformedClass.Kind.Changed, "ab12", List("Lib", "hello")),
      TransformedClass("lib.Outer$Inner", TransformedClass.Kind.Added, "cd34", List("Outer$Inner", "Inner", "$plus")),
      TransformedClass("lib.Gone", TransformedClass.Kind.Removed, "ef56", Nil)
    )
    TransformedClass.read(TransformedClass.write(classes)) shouldBe classes.sortBy(_.binaryName)
  }
}
