package com.example

object Main {
  fun greet(name: String): String = "Hello, $name!"
}

fun main() {
  println(Main.greet("World"))
}
