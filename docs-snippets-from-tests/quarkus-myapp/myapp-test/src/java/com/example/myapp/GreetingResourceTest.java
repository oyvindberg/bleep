package com.example.myapp;

import io.quarkus.test.common.http.TestHTTPResource;
import io.quarkus.test.junit.QuarkusTest;
import jakarta.inject.Inject;
import java.net.URL;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

//start
@QuarkusTest
class GreetingResourceTest {
  @TestHTTPResource("/hello")
  URL helloUrl;

  @Inject GreetingService service;

  @Test
  void helloEndpointServesGreeting() throws Exception {
    HttpResponse<String> response =
        HttpClient.newHttpClient()
            .send(
                HttpRequest.newBuilder(helloUrl.toURI()).GET().build(),
                HttpResponse.BodyHandlers.ofString());
    Assertions.assertEquals(200, response.statusCode());
    Assertions.assertEquals("Hello from Quarkus", response.body());
  }

  @Test
  void serviceIsInjectable() {
    Assertions.assertEquals("Hello from Quarkus", service.greeting());
  }
}
//stop
