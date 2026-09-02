package bleepscript;

import java.util.List;
import java.util.Objects;

/**
 * What a compile did, returned by {@link Commands#compile}.
 *
 * <p>The one thing a caller cannot get anywhere else is {@link #noOp()}: whether the compile
 * produced anything. bleep knows per project whether the compiler ran and found nothing to do, and
 * before this it had nowhere to report it, so a script that wanted to skip a deploy when nothing
 * had changed had no signal to test.
 *
 * @param noOp every project that compiled was already up to date, so no class file was rewritten. A
 *     run in which nothing compiled at all is not a no-op — there was no compile to be a no-op
 *     about, and saying otherwise would let a deploy skip on the strength of a run that never
 *     looked.
 * @param upToDateProjects the projects that reported nothing to do, named rather than counted
 * @param compilesCompleted how many project compiles finished, up to date or not
 */
public record CompileReport(
    boolean noOp, List<CrossProjectName> upToDateProjects, int compilesCompleted) {

  public CompileReport {
    Objects.requireNonNull(upToDateProjects, "upToDateProjects");
    upToDateProjects = List.copyOf(upToDateProjects);
  }
}
