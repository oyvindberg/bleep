package com.example

object Main {
  def greet(name: String): String = s"Hello, $name!"
}

@main def helloMain(): Unit =
  println(Main.greet("World"))
