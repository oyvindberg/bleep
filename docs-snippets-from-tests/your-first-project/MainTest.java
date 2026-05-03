package com.example;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class MainTest {
  @Test
  void greetsByName() {
    assertEquals("Hello, World!", Main.greet("World"));
  }
}
