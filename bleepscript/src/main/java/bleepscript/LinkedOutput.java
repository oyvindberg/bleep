package bleepscript;

import java.nio.file.Path;
import java.util.List;
import java.util.Objects;

/**
 * What one project's link wrote, as the linker reported it.
 *
 * <p>These paths come from the linker, not from listing the output directory afterwards. That is
 * the reason they are handed back at all: the layout under {@code link-output/} belongs to bleep
 * and has been renamed once already, so a script that rebuilds {@code
 * link-output/<mode>/js/main.js} by hand is depending on something bleep is free to change.
 *
 * @param project which project this link was for
 * @param platform the platform linked for, e.g. {@code Scala.js} or {@code Scala Native}
 * @param files every file the link wrote, in the order the linker reported — main artifact first,
 *     then source map and chunks
 */
public record LinkedOutput(CrossProjectName project, String platform, List<Path> files) {

  public LinkedOutput {
    Objects.requireNonNull(project, "project");
    Objects.requireNonNull(platform, "platform");
    Objects.requireNonNull(files, "files");
    files = List.copyOf(files);
  }

  /**
   * The linked program: the JavaScript module a JS link produced, or the executable a native link
   * produced. This is the path a script copies into a jar or serves.
   */
  public Path mainArtifact() {
    if (files.isEmpty()) {
      throw new IllegalStateException(
          project.name() + ": the " + platform + " link reported success but listed no files");
    }
    return files.get(0);
  }
}
