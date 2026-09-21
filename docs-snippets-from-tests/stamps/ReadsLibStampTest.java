package com.example;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.InputStream;
import java.util.Properties;
import org.junit.jupiter.api.Test;

public class ReadsLibStampTest {
  @Test
  void seesItsDependencysStamp() throws Exception {
    try (InputStream in = ReadsLibStampTest.class.getResourceAsStream("/bleep-stamp/lib.properties")) {
      assertNotNull(in, "lib's stamp is not on the test fork's classpath");
      Properties p = new Properties();
      p.load(in);
      assertTrue(p.getProperty("git-sha").matches("[0-9a-f]{40}"), "git-sha was " + p.getProperty("git-sha"));
    }
  }
}
