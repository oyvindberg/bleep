package com.example;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfEnvironmentVariable;
import static org.junit.jupiter.api.Assertions.assertEquals;

class GreeterIT {
  @Test
  @EnabledIfEnvironmentVariable(named = "RUN_INTEGRATION", matches = "true")
  void greetsAcrossLocales() {
    // Pretend this hits a Testcontainers Postgres or similar.
    assertEquals("Hello, world!", Greeter.hello("world"));
  }
}
