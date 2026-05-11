CREATE SEQUENCE IF NOT EXISTS items_id_seq;
CREATE TABLE IF NOT EXISTS items (
  id BIGINT PRIMARY KEY DEFAULT nextval('items_id_seq'),
  name VARCHAR NOT NULL,
  quantity INTEGER NOT NULL CHECK (quantity >= 0)
);
