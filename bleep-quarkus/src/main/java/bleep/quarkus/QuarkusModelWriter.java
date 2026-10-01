package bleep.quarkus;

import io.quarkus.bootstrap.model.ApplicationModel;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Forked by bleep with the app's own Quarkus version on the classpath. Reads a {@link ModelSpec}
 * file (argument 0), assembles the {@link ApplicationModel}, and serializes it to the spec's {@code
 * output} path in whatever format this Quarkus version's own bootstrap will read back.
 *
 * <p>Writes to a temp file and moves into place, so a killed fork can never leave a truncated model
 * where the cache would trust it.
 */
public final class QuarkusModelWriter {
  private QuarkusModelWriter() {}

  public static void main(String[] args) throws Exception {
    if (args.length != 1) {
      throw new IllegalArgumentException(
          "usage: QuarkusModelWriter <spec-file>, got " + args.length + " arguments");
    }
    ModelSpec spec = ModelSpec.parse(Path.of(args[0]));
    ApplicationModel model = new AppModelAssembler(spec).assemble();
    Files.createDirectories(spec.output.getParent());
    Path tmp = Files.createTempFile(spec.output.getParent(), "app-model", ".tmp");
    try {
      QuarkusCompat.serialize(model, tmp);
      Files.move(tmp, spec.output, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
    } finally {
      Files.deleteIfExists(tmp);
    }
  }
}
