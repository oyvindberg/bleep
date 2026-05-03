package com.example

import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe

class MainTest : FunSpec({
  test("greets by name") {
    Main.greet("World") shouldBe "Hello, World!"
  }
})
