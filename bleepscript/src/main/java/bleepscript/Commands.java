package bleepscript;

import java.util.List;
import java.util.Optional;

public interface Commands {
  /**
   * Compile {@code projects}. Throws when the compile fails; see {@link CompileReport} for what a
   * successful one reports back.
   */
  CompileReport compile(List<CrossProjectName> projects);

  CompileReport compile(List<CrossProjectName> projects, boolean watch);

  /**
   * Link {@code projects} — Scala.js, Scala Native, Kotlin/JS or Kotlin/Native — the way {@code
   * bleep link} does.
   *
   * <p>Without this a script that packages linked JavaScript into a jar had to shell out to the
   * {@code bleep} command line, paying the start-up cost a second time.
   *
   * <p>The returned {@link LinkReport} says where the link put things, taken from the linker rather
   * than from the directory layout — see {@link LinkedOutput}.
   */
  LinkReport link(List<CrossProjectName> projects, LinkOptions options);

  LinkReport link(List<CrossProjectName> projects, LinkOptions options, boolean watch);

  void test(List<CrossProjectName> projects);

  void test(
      List<CrossProjectName> projects,
      boolean watch,
      Optional<List<String>> only,
      Optional<List<String>> exclude);

  void run(CrossProjectName project);

  void run(
      CrossProjectName project,
      Optional<String> overrideMainClass,
      List<String> args,
      boolean raw,
      boolean watch);

  void clean(List<CrossProjectName> projects);

  void script(String scriptName, List<String> args);

  void script(String scriptName, List<String> args, boolean watch);

  /**
   * Publish to any supported target: local Ivy, local Maven folder, or a named resolver from {@code
   * bleep.yaml}. For local-only targets {@link #publishLocal(PublishOptions)} is a thinner
   * alternative.
   */
  void publish(PublishOptions options);

  void publish(PublishOptions options, boolean watch);

  /**
   * Publish to a local target only ({@link PublishTarget.LocalIvy} or {@link
   * PublishTarget.MavenFolder}). Convenience shorthand; for full target coverage use {@link
   * #publish(PublishOptions)}.
   */
  void publishLocal(PublishOptions options);

  void publishLocal(PublishOptions options, boolean watch);

  /**
   * Publish to Sonatype (Maven Central). PGP-signs every artifact via the caller's gpg agent,
   * bundles them, and uploads through the Sonatype Central Portal API. Reads {@code
   * SONATYPE_USERNAME} / {@code SONATYPE_PASSWORD} from the environment; reads {@code
   * sonatypeProfileName} / {@code sonatypeCredentialHost} from each project's {@code publish:}
   * block in {@code bleep.yaml}.
   *
   * <p>Equivalent to {@link #publish(PublishOptions)} with the target set to {@link
   * PublishTarget.SonatypeCentral}; this entry point is more direct when no other publish options
   * need configuring.
   */
  void publishSonatype(PublishOptions options);
}
