package bleepscript;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

public record RelPath(List<String> segments) {
  public RelPath {
    Objects.requireNonNull(segments, "segments");
    segments = List.copyOf(segments);
  }

  public static RelPath of(String... segments) {
    return new RelPath(Arrays.asList(segments));
  }

  public RelPath resolve(RelPath other) {
    List<String> combined = new ArrayList<>(segments.size() + other.segments.size());
    combined.addAll(segments);
    combined.addAll(other.segments);
    return new RelPath(combined);
  }

  public RelPath resolve(String segment) {
    List<String> combined = new ArrayList<>(segments.size() + 1);
    combined.addAll(segments);
    combined.add(segment);
    return new RelPath(combined);
  }

  public String asString() {
    return String.join("/", segments);
  }

  @Override
  public String toString() {
    return asString();
  }
}
