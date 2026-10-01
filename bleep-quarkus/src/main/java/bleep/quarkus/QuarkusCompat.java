package bleep.quarkus;

import io.quarkus.bootstrap.model.ApplicationModel;
import io.quarkus.bootstrap.model.ApplicationModelBuilder;
import io.quarkus.bootstrap.util.BootstrapUtils;
import io.quarkus.maven.dependency.ArtifactKey;
import java.lang.reflect.Method;
import java.nio.file.Path;
import java.util.Properties;

/**
 * The two places the bootstrap API drifted between the 3.15 compatibility floor this project
 * compiles against and current Quarkus. This JVM runs with the app's own Quarkus version on the
 * classpath, so each call probes what that version provides and dispatches accordingly — and blows
 * up with the probe results if a future version drops both shapes, rather than guessing.
 *
 * <ul>
 *   <li>{@code ApplicationModelBuilder.handleExtensionProperties} took {@code (Properties, String)}
 *       until 3.17, {@code (Properties, ArtifactKey)} after.
 *   <li>Serialization: old versions have only Java Object Serialization via {@code
 *       BootstrapUtils.serializeAppModel}, and their {@code BootstrapAppModelFactory} reads JOS
 *       back. Newer versions read via {@code ApplicationModelSerializer.deserialize}, which
 *       defaults to a JSON format — so where that class exists it MUST also be the one to write, or
 *       the test JVM will feed JOS bytes to a JSON parser.
 * </ul>
 */
final class QuarkusCompat {
  private QuarkusCompat() {}

  private static final String SERIALIZER_CLASS =
      "io.quarkus.bootstrap.app.ApplicationModelSerializer";

  static void handleExtensionProperties(
      ApplicationModelBuilder builder, Properties props, ArtifactKey extensionKey) {
    Method m;
    try {
      m =
          ApplicationModelBuilder.class.getMethod(
              "handleExtensionProperties", Properties.class, ArtifactKey.class);
    } catch (NoSuchMethodException newShape) {
      try {
        m =
            ApplicationModelBuilder.class.getMethod(
                "handleExtensionProperties", Properties.class, String.class);
      } catch (NoSuchMethodException oldShape) {
        throw new IllegalStateException(
            "This Quarkus version's ApplicationModelBuilder has neither known shape of"
                + " handleExtensionProperties",
            oldShape);
      }
    }
    try {
      Object second =
          m.getParameterTypes()[1] == ArtifactKey.class ? extensionKey : extensionKey.toGacString();
      m.invoke(builder, props, second);
    } catch (ReflectiveOperationException e) {
      throw new IllegalStateException("Failed to invoke " + m, e);
    }
  }

  static void serialize(ApplicationModel model, Path target) {
    Class<?> serializer;
    try {
      serializer = Class.forName(SERIALIZER_CLASS);
    } catch (ClassNotFoundException oldQuarkus) {
      // Pre-serializer-class Quarkus: JOS both ways.
      try {
        BootstrapUtils.serializeAppModel(model, target);
        return;
      } catch (Exception e) {
        throw new IllegalStateException("Failed to serialize application model to " + target, e);
      }
    }
    try {
      serializer
          .getMethod("serialize", ApplicationModel.class, Path.class)
          .invoke(null, model, target);
    } catch (ReflectiveOperationException e) {
      throw new IllegalStateException(
          SERIALIZER_CLASS + " exists but serialize(ApplicationModel, Path) failed", e);
    }
  }

  static ApplicationModel deserialize(Path source) {
    Class<?> serializer;
    try {
      serializer = Class.forName(SERIALIZER_CLASS);
    } catch (ClassNotFoundException oldQuarkus) {
      try {
        return BootstrapUtils.deserializeQuarkusModel(source);
      } catch (Exception e) {
        throw new IllegalStateException(
            "Failed to deserialize application model from " + source, e);
      }
    }
    try {
      return (ApplicationModel)
          serializer.getMethod("deserialize", Path.class).invoke(null, source);
    } catch (ReflectiveOperationException e) {
      throw new IllegalStateException(SERIALIZER_CLASS + " exists but deserialize(Path) failed", e);
    }
  }
}
