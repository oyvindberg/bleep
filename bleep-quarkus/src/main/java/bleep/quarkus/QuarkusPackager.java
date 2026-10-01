package bleep.quarkus;

import io.quarkus.bootstrap.app.CuratedApplication;
import io.quarkus.bootstrap.app.QuarkusBootstrap;
import io.quarkus.bootstrap.model.ApplicationModel;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Production build for a bleep-built Quarkus application: bootstrap in NORMAL mode from the
 * serialized model and run the augmentor's {@code createProductionApplication()}, which writes the
 * packaged output (fast-jar {@code quarkus-app/} by default, plus {@code
 * quarkus-artifact.properties} — the file {@code @QuarkusIntegrationTest} launches from).
 *
 * <p>Usage: {@code QuarkusPackager <serialized-model> <output-dir>}.
 */
public final class QuarkusPackager {
  private QuarkusPackager() {}

  public static void main(String[] args) throws Exception {
    if (args.length != 2) {
      throw new IllegalArgumentException(
          "usage: QuarkusPackager <serialized-model> <output-dir>, got "
              + args.length
              + " arguments");
    }
    Path modelFile = Path.of(args[0]);
    Path outputDir = Path.of(args[1]);
    Files.createDirectories(outputDir);

    ApplicationModel model = QuarkusCompat.deserialize(modelFile);

    try (CuratedApplication curated =
        QuarkusBootstrap.builder()
            .setExistingModel(model)
            .setBaseClassLoader(QuarkusPackager.class.getClassLoader())
            .setIsolateDeployment(true)
            .setMode(QuarkusBootstrap.Mode.PROD)
            .setApplicationRoot(model.getAppArtifact().getResolvedPaths())
            .setProjectRoot(model.getApplicationModule().getModuleDir().toPath())
            .setTargetDirectory(outputDir)
            .build()
            .bootstrap()) {
      curated.createAugmentor().createProductionApplication();
    }
  }
}
