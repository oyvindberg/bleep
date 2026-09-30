package scripts;

import bleepscript.BleepCodegenScript;
import bleepscript.CodegenTarget;
import bleepscript.Commands;
import bleepscript.Started;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.List;
import java.util.stream.Stream;

/**
 * Copies the files sbt or Maven generated when this build was imported, which are kept in
 * `scripts/resources/<cross project>`. A stand-in until it is replaced by code which generates them.
 */
public final class CopyImportedGeneratedFiles extends BleepCodegenScript {
  public CopyImportedGeneratedFiles() {
    super("CopyImportedGeneratedFiles");
  }

  @Override
  public void run(Started started, Commands commands, List<CodegenTarget> targets, List<String> args) {
    started.logger().warn("Copying files generated when this build was imported. Replace this script with code which generates them");
    Path kept = started.buildPaths().buildDir().resolve("scripts/resources");
    for (CodegenTarget target : targets) {
      Path project = kept.resolve(target.project().asString().replace('/', '-'));
      copy(project.resolve("sources"), target.sources());
      copy(project.resolve("resources"), target.resources());
    }
  }

  private static void copy(Path from, Path to) {
    if (!Files.isDirectory(from)) return;
    try (Stream<Path> files = Files.walk(from)) {
      for (Path file : (Iterable<Path>) files.filter(Files::isRegularFile)::iterator) {
        Path dest = to.resolve(from.relativize(file).toString());
        Files.createDirectories(dest.getParent());
        Files.copy(file, dest, StandardCopyOption.REPLACE_EXISTING);
      }
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }
}
