package com.example.myapp;

import java.util.List;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/items")
public class ItemController {
  private final ItemService service;

  public ItemController(ItemService service) {
    this.service = service;
  }

  @GetMapping
  public List<Item> all() {
    return service.all();
  }

  @PostMapping
  public ResponseEntity<Item> create(@RequestBody CreateRequest req) {
    return ResponseEntity.ok(service.create(req.name(), req.quantity()));
  }

  @PostMapping("/{id}/adjust")
  public ResponseEntity<Item> adjust(@PathVariable long id, @RequestBody AdjustRequest req) {
    return ResponseEntity.ok(service.adjust(id, req.delta()));
  }

  @ExceptionHandler(IllegalArgumentException.class)
  ResponseEntity<String> handleBadRequest(IllegalArgumentException e) {
    return ResponseEntity.badRequest().body(e.getMessage());
  }

  public record CreateRequest(String name, int quantity) {}

  public record AdjustRequest(int delta) {}
}
