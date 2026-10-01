package com.example.myapp;

import jakarta.inject.Inject;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;

//start
@Path("/hello")
public class GreetingResource {
  private final GreetingService service;

  @Inject
  public GreetingResource(GreetingService service) {
    this.service = service;
  }

  @GET
  @Produces(MediaType.TEXT_PLAIN)
  public String hello() {
    return service.greeting();
  }
}
//stop
