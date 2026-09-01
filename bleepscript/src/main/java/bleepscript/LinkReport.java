package bleepscript;

import java.util.List;
import java.util.Objects;

/** What a link did, returned by {@link Commands#link}. */
public record LinkReport(List<LinkedOutput> outputs) {

  public LinkReport {
    Objects.requireNonNull(outputs, "outputs");
    outputs = List.copyOf(outputs);
  }

  /**
   * The output for one project.
   *
   * <p>Throws rather than returning an empty {@link java.util.Optional}: a caller asking for a
   * project it just linked and getting nothing back has a broken build, and handing it an empty
   * value only moves the moment it finds out.
   */
  public LinkedOutput forProject(CrossProjectName project) {
    return outputs.stream()
        .filter(o -> o.project().equals(project))
        .findFirst()
        .orElseThrow(
            () ->
                new IllegalArgumentException(
                    "no link output for "
                        + project.name()
                        + "; this link produced: "
                        + outputs.stream().map(o -> o.project().name()).toList()));
  }
}
