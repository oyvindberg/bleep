package com.example

class MainTest extends munit.FunSuite {
  test("greets by name") {
    assertEquals(Main.greet("World"), "Hello, World!")
  }
}
