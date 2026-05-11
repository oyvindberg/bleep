package com.example.myapp;

import java.util.List;
import java.util.Optional;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.stereotype.Repository;

@Repository
public class ItemRepository {
  private static final RowMapper<Item> ROW =
      (rs, n) -> new Item(rs.getLong("id"), rs.getString("name"), rs.getInt("quantity"));

  private final JdbcTemplate jdbc;

  public ItemRepository(JdbcTemplate jdbc) {
    this.jdbc = jdbc;
  }

  public List<Item> findAll() {
    return jdbc.query("SELECT id, name, quantity FROM items ORDER BY id", ROW);
  }

  public Optional<Item> findById(long id) {
    return jdbc.query("SELECT id, name, quantity FROM items WHERE id = ?", ROW, id).stream()
        .findFirst();
  }

  public Item insert(String name, int quantity) {
    Long id =
        jdbc.queryForObject(
            "INSERT INTO items (name, quantity) VALUES (?, ?) RETURNING id",
            Long.class,
            name,
            quantity);
    return new Item(id, name, quantity);
  }

  public Item update(Item item) {
    jdbc.update(
        "UPDATE items SET name = ?, quantity = ? WHERE id = ?",
        item.name(),
        item.quantity(),
        item.id());
    return item;
  }
}
