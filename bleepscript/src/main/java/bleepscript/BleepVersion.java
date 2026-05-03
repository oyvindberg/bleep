package bleepscript;

import java.util.Objects;

public record BleepVersion(String value) {
  public BleepVersion {
    Objects.requireNonNull(value, "value");
  }

  @Override
  public String toString() {
    return value;
  }
}
