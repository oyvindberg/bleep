package bleepscript;

import java.util.Optional;

public record CompileSetup(
    Optional<CompileOrder> order,
    Optional<Boolean> addLibraryToBootClasspath,
    Optional<Boolean> addCompilerToClasspath,
    Optional<Boolean> addExtraJarsToClasspath,
    Optional<Boolean> manageBootClasspath,
    Optional<Boolean> filterLibraryFromClasspath) {

  public enum CompileOrder {
    Mixed,
    JavaThenScala,
    ScalaThenJava
  }
}
