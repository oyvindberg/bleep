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

  test("Java model with annotation processing") {
    // Test default case (no annotation processing)
    roundtrip(Java(Options.empty))

    // Test with annotation processing disabled explicitly
    roundtrip(Java(Options.empty, Some(AnnotationProcessing(false))))

    // Test with annotation processing enabled
    roundtrip(Java(Options.empty, Some(AnnotationProcessing(true))))

    // Test with options and annotation processing
    val opts = Options.parse(List("-Xlint:all", "-Werror"), maybeRelativize = None)
    roundtrip(Java(opts, Some(AnnotationProcessing(true))))
  }

  test("Java model setlike operations") {
    val java1 = Java(Options.parse(List("-Xlint:all"), maybeRelativize = None), Some(AnnotationProcessing(true)))
    val java2 = Java(Options.parse(List("-Werror"), maybeRelativize = None), Some(AnnotationProcessing(false)))

    // Test union
    val union = java1.union(java2)
    assert(union.options.render === List("-Werror", "-Xlint:all"))
    assert(union.annotationProcessing === Some(AnnotationProcessing(false))) // java2's value wins

    // Test intersect
    val intersect = java1.intersect(java2)
    assert(intersect.options.isEmpty)
    assert(intersect.annotationProcessing === Some(AnnotationProcessing(false))) // true && false = false

    // Test removeAll
    val removed = java1.removeAll(java1)
    assert(removed.options.isEmpty)
    assert(removed.annotationProcessing === None) // Same values result in None
  }
}
