package bleepscript;

/**
 * Marks a JAR's role within a published library: the main classes JAR, a sources JAR, or a Javadoc
 * JAR.
 */
public enum JarType {
  JAR,
  SOURCES_JAR,
  DOCS_JAR
}
