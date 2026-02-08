package bleep.analysis

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.Path
import java.util.jar.JarFile
import scala.jdk.CollectionConverters.*
import scala.collection.mutable
import org.objectweb.asm.{AnnotationVisitor, ClassReader, ClassVisitor, Opcodes}
import coursier._
import coursier.parse.DependencyParser

/** Exploration test for kotlin-metadata-jvm API from Scala */
class KotlinMetadataExploreTest extends AnyFunSuite with Matchers {

  def fetchClasspath(deps: Seq[String]): Seq[Path] = {
    val parsed = deps.flatMap { depStr =>
      DependencyParser.dependency(depStr, scala.util.Properties.versionNumberString) match {
        case Left(err)  => throw new RuntimeException(s"Failed to parse: $err")
        case Right(dep) => Some(dep)
      }
    }
    Fetch().addDependencies(parsed*).run().map(_.toPath)
  }

  test("explore kotlin-metadata-jvm API") {
    val classpath = fetchClasspath(
      Seq(
        "org.jetbrains.kotlinx:kotlinx-coroutines-core-jvm:1.7.3"
      )
    )

    val jarPath = classpath.find(_.toString.contains("kotlinx-coroutines-core-jvm")).get
    val jar = new JarFile(jarPath.toFile)

    try {
      // Find the Job interface class file specifically
      val entries = jar.entries().asScala.toList
      val classEntry = entries.find(e => e.getName == "kotlinx/coroutines/Job.class").get

      info(s"Analyzing: ${classEntry.getName}")

      val is = jar.getInputStream(classEntry)
      val classBytes = is.readAllBytes()
      is.close()

      // Extract @kotlin.Metadata annotation values using ASM
      var kind: Int = 0
      var metadataVersion: Array[Int] = Array.empty
      var data1: Array[String] = Array.empty
      var data2: Array[String] = Array.empty
      var extraString: String = ""
      var packageName: String = ""
      var extraInt: Int = 0

      val reader = new ClassReader(classBytes)
      val visitor = new ClassVisitor(Opcodes.ASM9) {
        override def visitAnnotation(descriptor: String, visible: Boolean): AnnotationVisitor =
          if descriptor == "Lkotlin/Metadata;" then
            new AnnotationVisitor(Opcodes.ASM9) {
              override def visit(name: String, value: Any): Unit =
                name match {
                  case "k"  => kind = value.asInstanceOf[Int]
                  case "xi" => extraInt = value.asInstanceOf[Int]
                  case "xs" => extraString = if value != null then value.asInstanceOf[String] else ""
                  case "pn" => packageName = if value != null then value.asInstanceOf[String] else ""
                  case "mv" => metadataVersion = value.asInstanceOf[Array[Int]]
                  case "bv" => () // bytecode version - ignore
                  case _    => ()
                }

              override def visitArray(name: String): AnnotationVisitor =
                name match {
                  case "d1" =>
                    new AnnotationVisitor(Opcodes.ASM9) {
                      private val values = mutable.ArrayBuffer[String]()
                      override def visit(n: String, v: Any): Unit =
                        values += v.asInstanceOf[String]
                      override def visitEnd(): Unit =
                        data1 = values.toArray
                    }
                  case "d2" =>
                    new AnnotationVisitor(Opcodes.ASM9) {
                      private val values = mutable.ArrayBuffer[String]()
                      override def visit(n: String, v: Any): Unit =
                        values += v.asInstanceOf[String]
                      override def visitEnd(): Unit =
                        data2 = values.toArray
                    }
                  case _ => null
                }
            }
          else null
      }

      reader.accept(visitor, ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG)

      info(s"kind: $kind")
      info(s"metadataVersion: ${metadataVersion.mkString(".")}")
      info(s"data1 length: ${data1.length}")
      info(s"data2 length: ${data2.length}")

      // Now try to use kotlin-metadata-jvm
      import kotlin.metadata.jvm.{JvmMetadataUtil, KotlinClassMetadata}
      import kotlin.metadata.Attributes

      // Create Metadata using JvmMetadataUtil.Metadata()
      val metadata = JvmMetadataUtil.Metadata(
        kind,
        metadataVersion,
        data1,
        data2,
        extraString,
        packageName,
        extraInt
      )

      // Read the metadata using Companion object
      val kcm = KotlinClassMetadata.Companion.readLenient(metadata)
      info(s"KotlinClassMetadata type: ${kcm.getClass.getSimpleName}")

      kcm match {
        case cls: KotlinClassMetadata.Class =>
          // Access kmClass
          val kmClass = cls.getKmClass
          info(s"Class name: ${kmClass.getName}")

          // Get supertypes
          val supertypes = kmClass.getSupertypes
          info(s"Supertypes count: ${supertypes.size}")
          supertypes.asScala.foreach { st =>
            val classifier = st.getClassifier
            info(s"  Supertype classifier: $classifier")
          }

          // Get functions
          val functions = kmClass.getFunctions
          info(s"Functions count: ${functions.size}")
          functions.asScala.take(5).foreach { func =>
            info(s"  Function: ${func.getName}")
            // Access visibility via Attributes static method
            val visibility = Attributes.getVisibility(func)
            info(s"    Visibility: $visibility")
            // Check if suspend via Attributes
            val isSuspend = Attributes.isSuspend(func)
            info(s"    isSuspend: $isSuspend")
          }

          // Get properties
          val properties = kmClass.getProperties
          info(s"Properties count: ${properties.size}")
          properties.asScala.take(5).foreach { prop =>
            info(s"  Property: ${prop.getName}")
            val returnType = prop.getReturnType
            // Check nullable via Attributes
            info(s"    Return type nullable: ${Attributes.isNullable(returnType)}")
          }

        case _ =>
          info(s"Not a class metadata: ${kcm.getClass}")
      }

      succeed
    } finally jar.close()
  }
}
