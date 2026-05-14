package bleepscript;

/**
 * The repository layout for published artifacts: Maven (jar, sources, javadoc, pom) or Ivy (jar,
 * sources, ivy.xml, pom, docs).
 */
public sealed interface PublishLayout permits PublishLayout.Maven, PublishLayout.Ivy {

  /** Maven-style layout. */
  final class Maven implements PublishLayout {
    public static final Maven INSTANCE = new Maven();

    private Maven() {}
  }

  /** Ivy-style layout. */
  final class Ivy implements PublishLayout {
    public static final Ivy INSTANCE = new Ivy();

    private Ivy() {}
  }
}
