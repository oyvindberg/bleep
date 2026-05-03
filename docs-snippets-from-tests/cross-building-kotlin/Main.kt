package com.example

fun main() {
  val green = "\u001B[32m"
  val cyan = "\u001B[36m"
  val reset = "\u001B[0m"
  println("${green}Hello${reset} from ${cyan}bleep${reset}!")
}
