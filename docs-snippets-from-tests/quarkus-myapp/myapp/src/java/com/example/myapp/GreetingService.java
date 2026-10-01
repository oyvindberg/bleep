package com.example.myapp;

import jakarta.enterprise.context.ApplicationScoped;

//start
@ApplicationScoped
public class GreetingService {
  public String greeting() {
    return "Hello from Quarkus";
  }
}
//stop
