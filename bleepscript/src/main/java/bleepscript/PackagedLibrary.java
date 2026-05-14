package bleepscript;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/**
 * An immutable bundle of artifacts ready to publish. Mirrors {@code bleep.packaging.MavenLayout}
 * and {@code IvyLayout}: the JAR, sources JAR, POM, javadoc JAR are named explicitly, with an extra
 * {@code ivyFile} slot populated only for Ivy-layout publications.
 *
 * <p>The {@code files()} map is shallow-copied to be unmodifiable. The {@code byte[]} values are
 * not deeply copied; callers should treat them as immutable.
 *
 * <p>Common post-package mutation pattern (e.g. ProGuard shrink): take {@link #jarFile()},
 * transform the bytes, hand back via {@link #withJarFile(byte[])}, then publish.
 */
public record PackagedLibrary(
    Dep coords,
    RelPath jarFilePath,
    RelPath sourceFilePath,
    RelPath pomFilePath,
    RelPath docFilePath,
    Optional<RelPath> ivyFilePath,
    Map<RelPath, byte[]> files) {

  public PackagedLibrary {
    Objects.requireNonNull(coords, "coords");
    Objects.requireNonNull(jarFilePath, "jarFilePath");
    Objects.requireNonNull(sourceFilePath, "sourceFilePath");
    Objects.requireNonNull(pomFilePath, "pomFilePath");
    Objects.requireNonNull(docFilePath, "docFilePath");
    Objects.requireNonNull(ivyFilePath, "ivyFilePath");
    Objects.requireNonNull(files, "files");
    requirePresent(files, jarFilePath, "jarFilePath");
    requirePresent(files, sourceFilePath, "sourceFilePath");
    requirePresent(files, pomFilePath, "pomFilePath");
    requirePresent(files, docFilePath, "docFilePath");
    if (ivyFilePath.isPresent()) {
      requirePresent(files, ivyFilePath.get(), "ivyFilePath");
    }
    files = Map.copyOf(files);
  }

  /** Bytes of the main classes JAR. */
  public byte[] jarFile() {
    return files.get(jarFilePath);
  }

  /** Bytes of the sources JAR. */
  public byte[] sourceFile() {
    return files.get(sourceFilePath);
  }

  /** Bytes of the POM. */
  public byte[] pomFile() {
    return files.get(pomFilePath);
  }

  /** Bytes of the javadoc JAR. */
  public byte[] docFile() {
    return files.get(docFilePath);
  }

  /** Bytes of the ivy.xml descriptor; empty for Maven-layout publications. */
  public Optional<byte[]> ivyFile() {
    return ivyFilePath.map(files::get);
  }

  /** Return a copy with the same named paths but a fully replaced file set. */
  public PackagedLibrary withFiles(Map<RelPath, byte[]> newFiles) {
    return new PackagedLibrary(
        coords, jarFilePath, sourceFilePath, pomFilePath, docFilePath, ivyFilePath, newFiles);
  }

  /** Return a copy with the main JAR bytes replaced, everything else preserved. */
  public PackagedLibrary withJarFile(byte[] newJarBytes) {
    Objects.requireNonNull(newJarBytes, "newJarBytes");
    Map<RelPath, byte[]> next = new HashMap<>(files);
    next.put(jarFilePath, newJarBytes);
    return withFiles(next);
  }

  private static void requirePresent(Map<RelPath, byte[]> files, RelPath path, String fieldName) {
    if (!files.containsKey(path)) {
      throw new IllegalArgumentException(
          fieldName + " " + path.asString() + " is not present in files");
    }
  }
}
