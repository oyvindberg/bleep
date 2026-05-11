package com.example.myapp;

import java.util.List;
import org.springframework.stereotype.Service;

@Service
public class ItemService {
  private final ItemRepository repo;

  public ItemService(ItemRepository repo) {
    this.repo = repo;
  }

  public List<Item> all() {
    return repo.findAll();
  }

  public Item create(String name, int quantity) {
    if (name == null || name.isBlank()) {
      throw new IllegalArgumentException("name is required");
    }
    if (quantity < 0) {
      throw new IllegalArgumentException("quantity must be non-negative");
    }
    return repo.insert(name, quantity);
  }

  public Item adjust(long id, int delta) {
    Item current =
        repo.findById(id).orElseThrow(() -> new IllegalArgumentException("no item " + id));
    int next = current.quantity() + delta;
    if (next < 0) {
      throw new IllegalArgumentException("adjust would push quantity below zero");
    }
    return repo.update(new Item(current.id(), current.name(), next));
  }
}
