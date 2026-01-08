package bleep
package model

import io.circe.syntax.*
import org.scalactic.TripleEqualsSupport
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

class JavaTest extends AnyFunSuite with TripleEqualsSupport {
  def roundtrip(java: Java): Assertion = {
    val json = java.asJson
    val java2 = json.as[Java].orThrowWithError("Failed to parse json")
    assert(java === java2)
  }

  test("Java model with annotation processors") {
    // Test default case (no annotation processors)
    roundtrip(Java(Options.empty))

    // Test with empty annotation processors
    roundtrip(Java(Options.empty, JsonSet.empty))

    // Test with single annotation processor
    val processor1 = AnnotationProcessor("com.example.MyProcessor")
    roundtrip(Java(Options.empty, JsonSet(processor1)))

    // Test with multiple annotation processors
    val processor2 = AnnotationProcessor("com.example.AnotherProcessor")
    roundtrip(Java(Options.empty, JsonSet(processor1, processor2)))

    // Test with options and annotation processors
    val opts = Options.parse(List("-Xlint:all", "-Werror"), maybeRelativize = None)
    roundtrip(Java(opts, JsonSet(processor1)))
  }

  test("Java model setlike operations") {
    val processor1 = AnnotationProcessor("com.example.Processor1")
    val processor2 = AnnotationProcessor("com.example.Processor2")
    val processor3 = AnnotationProcessor("com.example.Processor3")

    val java1 = Java(Options.parse(List("-Xlint:all"), maybeRelativize = None), JsonSet(processor1, processor2))
    val java2 = Java(Options.parse(List("-Werror"), maybeRelativize = None), JsonSet(processor2, processor3))

    // Test union
    val union = java1.union(java2)
    assert(union.options.render === List("-Werror", "-Xlint:all"))
    assert(union.annotationProcessors.values.toSet === Set(processor1, processor2, processor3))

    // Test intersect
    val intersect = java1.intersect(java2)
    assert(intersect.options.isEmpty)
    assert(intersect.annotationProcessors.values.toSet === Set(processor2)) // Only processor2 is in both

    // Test removeAll
    val removed = java1.removeAll(java1)
    assert(removed.options.isEmpty)
    assert(removed.annotationProcessors.isEmpty)
  }
}
