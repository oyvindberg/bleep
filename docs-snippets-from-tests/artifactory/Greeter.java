package com.test;

public final class Greeter {
  private Greeter() {}

  public static String hello(String name) {
    return "Hello, " + name + "!";
  }
}
