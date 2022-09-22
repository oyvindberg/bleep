package com.foo

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.funsuite.AnyFunSuite

class AppTest extends AnyFunSuite with TypeCheckedTripleEquals {
  test("works") {
    assert(App.greeting("a").plainText === "Hello, a")
  }
}
