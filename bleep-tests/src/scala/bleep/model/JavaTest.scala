package bleep
package model

import io.circe.syntax.*
import org.scalactic.TripleEqualsSupport
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.SortedMap

class JavaTest extends AnyFunSuite with TripleEqualsSupport {
  private def java(
      options: Options = Options.empty,
      scan: Option[Boolean] = None,
      processors: JsonSet[Dep] = JsonSet.empty,
      processorOptions: AnnotationProcessorOptions = AnnotationProcessorOptions.empty
  ): Java =
    Java(
      options = options,
      scanForAnnotationProcessors = scan,
      annotationProcessors = processors,
      annotationProcessorOptions = processorOptions
    )

  def roundtrip(j: Java): Assertion = {
    val json = j.asJson
    val j2 = json.as[Java].orThrowWithError("Failed to parse json")
    assert(j === j2)
  }

  private val lombok = Dep.Java("org.projectlombok", "lombok", "1.18.30")
  private val autoValue = Dep.Java("com.google.auto.value", "auto-value", "1.10.4")

  test("Java model annotation processor round-trip") {
    roundtrip(java())
    roundtrip(java(scan = Some(true)))
    roundtrip(java(scan = Some(false)))
    roundtrip(java(processors = JsonSet(lombok)))
    roundtrip(java(processors = JsonSet(lombok, autoValue)))
    roundtrip(
      java(
        scan = Some(true),
        processors = JsonSet(autoValue),
        processorOptions = AnnotationProcessorOptions(SortedMap("autovalue.no_assigned" -> "true"))
      )
    )
  }

  test("Java model setlike — annotationProcessors union/intersect/removeAll") {
    val j1 = java(processors = JsonSet(lombok, autoValue))
    val j2 = java(processors = JsonSet(autoValue))

    assert(j1.union(j2).annotationProcessors.values === Set(lombok, autoValue))
    assert(j1.intersect(j2).annotationProcessors.values === Set(autoValue))
    assert(j1.removeAll(j2).annotationProcessors.values === Set(lombok))
    assert(j1.removeAll(j1).annotationProcessors.values === Set.empty[Dep])
  }

  test("Java model setlike — annotationProcessorOptions union prefers child") {
    val parent = java(processorOptions = AnnotationProcessorOptions(SortedMap("k1" -> "parent", "k2" -> "shared")))
    val child = java(processorOptions = AnnotationProcessorOptions(SortedMap("k1" -> "child", "k3" -> "child-only")))

    val merged = child.union(parent)
    assert(merged.annotationProcessorOptions.value === SortedMap("k1" -> "child", "k2" -> "shared", "k3" -> "child-only"))
  }

  test("Java model setlike — scanForAnnotationProcessors union/intersect/removeAll") {
    val tEnabled = java(scan = Some(true))
    val tDisabled = java(scan = Some(false))
    val tUnset = java(scan = None)

    assert(tEnabled.union(tDisabled).scanForAnnotationProcessors === Some(true)) // self wins via orElse
    assert(tUnset.union(tEnabled).scanForAnnotationProcessors === Some(true))
    assert(tEnabled.intersect(tEnabled).scanForAnnotationProcessors === Some(true))
    assert(tEnabled.intersect(tDisabled).scanForAnnotationProcessors === None)
    assert(tEnabled.removeAll(tEnabled).scanForAnnotationProcessors === None)
    assert(tEnabled.removeAll(tDisabled).scanForAnnotationProcessors === Some(true))
  }
}
