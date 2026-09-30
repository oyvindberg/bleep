package bleep
package model

import org.scalatest.funsuite.AnyFunSuite

class LibraryVersionSchemeTest extends AnyFunSuite {
  // http4s sets `"org.scala-native" %% "test-interface_native0.5" % "always"`. On a native project that names the module without a platform suffix, which is
  // `forceJvm`, and on a jvm project it doesn't need to. Both must survive in one set, or native projects get the jvm one and name the wrong module
  test("schemes differing only in forceJvm are different schemes") {
    val jvm = LibraryVersionScheme.from(Dep.Scala("org.scala-native", "test-interface_native0.5", "always")).fold(err => fail(err), identity)
    val native = jvm.copy(dep = jvm.dep.mapScala(_.copy(forceJvm = true)))
    assert(jvm.dep.repr == native.dep.repr)

    val both = JsonSet(jvm, native)
    assert(both.values.size == 2)
    assert(both.removeAll(JsonSet(jvm)).values.toList == List(native))
  }
}
