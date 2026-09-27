package bleep.bsp

import bleep.Checksums
import bleep.analysis.TransformedClass
import org.objectweb.asm.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** What a post-compile transform did to the API, class by class: the classes it changed, added and removed, as consumers' zinc is told about them.
  *
  * Consumers compile against the transform's output (`classes`), but zinc's analysis describes the compiler's (`classes-pre`). For every class the transform
  * left alone they are the same, and zinc's own invalidation is exact. The difference is what this computes — per class, so zinc can go on invalidating per
  * class: see [[bleep.analysis.OutputDeterminants]].
  *
  * A class is reduced to a set of API facts, from its class file — the header in parts (access, super class, each interface, generic signature, each
  * annotation, each non-code attribute, inner-class and nest entries, permitted subclasses, record components) and each non-private field and method (access,
  * name, descriptor, signature, exceptions, annotations, parameter annotations, annotation default) — and from its `.tasty` file, by content: Scala 3 reads its
  * API there, not from the class file. A class's delta is the facts in `classes` but not in `classes-pre` and those in `classes-pre` but not in `classes`.
  *
  *   - A transform that does not touch the API — coverage instrumentation adding private synthetic members, a body-only enhancement, a checker that copies —
  *     changes no class, and no consumer recompiles, however often it or its script changes.
  *   - A transform that does touch it changes those classes only, and only when its contribution to them changes: an edit to a transformed class's own source
  *     puts the same new fact on both sides, which cancels, and zinc handles it the usual way.
  *
  * Private members are left out: nothing outside the class can link against them. Code is left out: consumers never see it. Anything but class and TASTy files
  * is left out: a consumer's compiler reads nothing else.
  */
object PostCompileAbi {

  def delta(compilerOutput: Path, transformed: Path): List[TransformedClass] = {
    val before = classesIn(compilerOutput)
    val after = classesIn(transformed)
    (before.keySet ++ after.keySet).toList.sorted.flatMap { binaryName =>
      (before.get(binaryName), after.get(binaryName)) match {
        case (Some(b), Some(a)) =>
          val added = a.facts -- b.facts
          val removed = b.facts -- a.facts
          if (added.isEmpty && removed.isEmpty) None
          else Some(TransformedClass(binaryName, TransformedClass.Kind.Changed, hash(added, removed), a.names))
        case (None, Some(a)) => Some(TransformedClass(binaryName, TransformedClass.Kind.Added, hash(a.facts, Set.empty), a.names))
        case (Some(b), None) => Some(TransformedClass(binaryName, TransformedClass.Kind.Removed, hash(Set.empty, b.facts), b.names))
        case (None, None)    => None
      }
    }
  }

  private case class ClassApi(facts: Set[String], names: List[String])

  private def hash(added: Set[String], removed: Set[String]): String = {
    val md = MessageDigest.getInstance("SHA-256")
    def mix(s: String): Unit = {
      md.update(s.getBytes(StandardCharsets.UTF_8))
      md.update(0.toByte)
    }
    added.toList.sorted.foreach(f => mix("+" + f))
    removed.toList.sorted.foreach(f => mix("-" + f))
    Checksums.byteArrayToHexString(md.digest())
  }

  /** Every class under `dir`, by binary name: its class file's facts together with its `.tasty` file's content. */
  private def classesIn(dir: Path): Map[String, ClassApi] =
    if (!Files.isDirectory(dir)) Map.empty
    else {
      val files = {
        val stream = Files.walk(dir)
        try stream.iterator().asScala.filter(Files.isRegularFile(_)).toList
        finally stream.close()
      }
      val perFile: List[(String, ClassApi)] = files.flatMap { f =>
        val rel = dir.relativize(f).toString.replace('\\', '/')
        if (rel.endsWith(".class")) Some(rel.stripSuffix(".class").replace('/', '.') -> classApi(Files.readAllBytes(f)))
        else if (rel.endsWith(".tasty")) {
          val content = Checksums.byteArrayToHexString(MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(f)))
          Some(rel.stripSuffix(".tasty").replace('/', '.') -> ClassApi(Set(s"tasty $content"), Nil))
        } else None
      }
      perFile.groupMapReduce(_._1)(_._2)((a, b) => ClassApi(a.facts ++ b.facts, (a.names ++ b.names).distinct))
    }

  private def classApi(bytes: Array[Byte]): ClassApi = {
    val facts = mutable.Set.empty[String]
    val names = mutable.LinkedHashSet.empty[String]

    def annotationValues(owner: String, into: mutable.ListBuffer[String]): AnnotationVisitor =
      new AnnotationVisitor(Opcodes.ASM9) {
        override def visit(name: String, value: Any): Unit = into += s"$name=${render(value)}"
        override def visitEnum(name: String, descriptor: String, value: String): Unit = into += s"$name=$descriptor.$value"
        override def visitAnnotation(name: String, descriptor: String): AnnotationVisitor = {
          into += s"$name=@$descriptor"
          annotationValues(s"$owner.$name", into)
        }
        override def visitArray(name: String): AnnotationVisitor = {
          into += s"$name=[]"
          annotationValues(s"$owner.$name[]", into)
        }
      }
    def annotation(owner: String, desc: String, visible: Boolean): AnnotationVisitor = {
      val values = mutable.ListBuffer.empty[String]
      facts += s"$owner annotation $desc visible=$visible"
      new AnnotationVisitor(Opcodes.ASM9, annotationValues(owner, values)) {
        override def visitEnd(): Unit = facts += s"$owner annotation $desc values ${values.mkString(",")}"
      }
    }

    new ClassReader(bytes).accept(
      new ClassVisitor(Opcodes.ASM9) {
        override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): Unit = {
          facts += s"class $name access $access"
          facts += s"class super $superName"
          facts += s"class signature $signature"
          interfaces.foreach(i => facts += s"class implements $i")
          val simple = name.substring(name.lastIndexOf('/') + 1)
          names += simple
          names += simple.substring(simple.lastIndexOf('$') + 1)
        }
        override def visitAnnotation(desc: String, visible: Boolean): AnnotationVisitor = annotation("class", desc, visible)
        override def visitAttribute(attr: Attribute): Unit = facts += s"class attribute ${attr.`type`} ${attributeContent(attr)}"
        override def visitInnerClass(name: String, outerName: String, innerName: String, access: Int): Unit =
          facts += s"class inner $name outer $outerName as $innerName access $access"
        override def visitNestHost(nestHost: String): Unit = facts += s"class nest host $nestHost"
        override def visitNestMember(nestMember: String): Unit = facts += s"class nest member $nestMember"
        override def visitOuterClass(owner: String, name: String, descriptor: String): Unit = facts += s"class enclosed by $owner $name $descriptor"
        override def visitPermittedSubclass(permittedSubclass: String): Unit = facts += s"class permits $permittedSubclass"
        override def visitRecordComponent(name: String, descriptor: String, signature: String): RecordComponentVisitor = {
          facts += s"record component $name $descriptor $signature"
          names += name
          null
        }
        override def visitField(access: Int, name: String, desc: String, signature: String, value: Any): FieldVisitor =
          if ((access & Opcodes.ACC_PRIVATE) != 0) null
          else {
            val owner = s"field $name $desc"
            facts += s"$owner access $access signature $signature constant ${render(value)}"
            names += name
            new FieldVisitor(Opcodes.ASM9) {
              override def visitAnnotation(d: String, visible: Boolean): AnnotationVisitor = annotation(owner, d, visible)
            }
          }
        override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor =
          if ((access & Opcodes.ACC_PRIVATE) != 0) null
          else {
            val owner = s"method $name$desc"
            facts += s"$owner access $access signature $signature throws ${Option(exceptions).fold("")(_.mkString(","))}"
            names += name
            new MethodVisitor(Opcodes.ASM9) {
              override def visitAnnotation(d: String, visible: Boolean): AnnotationVisitor = annotation(owner, d, visible)
              override def visitParameterAnnotation(parameter: Int, d: String, visible: Boolean): AnnotationVisitor =
                annotation(s"$owner parameter $parameter", d, visible)
              override def visitAnnotationDefault(): AnnotationVisitor = {
                val values = mutable.ListBuffer.empty[String]
                new AnnotationVisitor(Opcodes.ASM9, annotationValues(owner, values)) {
                  override def visitEnd(): Unit = facts += s"$owner default ${values.mkString(",")}"
                }
              }
            }
          }
      },
      Array[Attribute](new RawAttribute("TASTY"), new RawAttribute("ScalaSig"), new RawAttribute("ScalaInlineInfo"), new RawAttribute("Scala")),
      ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES
    )
    ClassApi(facts.toSet, names.toList)
  }

  /** Keeps the raw bytes of a non-standard class attribute (TASTy's UUID, Scala 2's pickle), which consumers read as API. */
  private final class RawAttribute(tpe: String, val content: Array[Byte]) extends Attribute(tpe) {
    def this(tpe: String) = this(tpe, Array.emptyByteArray)
    override def read(cr: ClassReader, off: Int, len: Int, buf: Array[Char], codeOff: Int, labels: Array[Label]): Attribute =
      new RawAttribute(tpe, Array.tabulate[Byte](len)(i => cr.readByte(off + i).toByte))
  }

  private def attributeContent(attr: Attribute): String = attr match {
    case raw: RawAttribute => Checksums.byteArrayToHexString(raw.content)
    case other             => other.`type`
  }

  private def render(value: Any): String = value match {
    case null               => "null"
    case bytes: Array[Byte] => Checksums.byteArrayToHexString(bytes)
    case arr: Array[?]      => arr.map(render).mkString("[", ",", "]")
    case other              => other.toString
  }
}
