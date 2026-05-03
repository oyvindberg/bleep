package mylib

class GreetingTest extends munit.FunSuite {
  test("hello") {
    assertEquals(Greeting.hello("world"), "Hello, world!")
  }
}
