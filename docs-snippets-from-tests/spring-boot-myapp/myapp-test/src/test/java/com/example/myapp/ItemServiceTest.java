package com.example.myapp;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

@SpringBootTest
@ActiveProfiles("test")
class ItemServiceTest {
  @Autowired ItemService service;

  @Test
  void createsAndListsItems() {
    Item bread = service.create("bread", 5);
    assertNotNull(bread.id());
    assertEquals("bread", bread.name());
    assertEquals(5, bread.quantity());

    Item milk = service.create("milk", 2);

    List<Item> all = service.all();
    assertTrue(all.contains(bread));
    assertTrue(all.contains(milk));
  }

  @Test
  void adjustsQuantity() {
    Item it = service.create("water", 10);
    Item after = service.adjust(it.id(), -3);
    assertEquals(7, after.quantity());
  }

  @Test
  void rejectsNegativeQuantityOnAdjust() {
    Item it = service.create("egg", 1);
    assertThrows(IllegalArgumentException.class, () -> service.adjust(it.id(), -2));
  }

  @Test
  void rejectsBlankName() {
    assertThrows(IllegalArgumentException.class, () -> service.create("  ", 1));
  }
}
