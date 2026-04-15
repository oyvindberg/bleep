package bleepscript;

/**
 * Internal helper used by {@link BleepScript#main} and {@link BleepCodegenScript#main} to discover
 * the launched class and instantiate it reflectively.
 */
final class Launcher {
  private Launcher() {}

  static <T> T instantiate(Class<T> expected) throws ReflectiveOperationException {
    String command = System.getProperty("sun.java.command");
    if (command == null || command.isBlank()) {
      throw new IllegalStateException(
          "BleepScript's inherited main requires the 'sun.java.command' system property, which is"
              + " not set in this JVM. Either use a standard JDK launcher (HotSpot, GraalVM,"
              + " OpenJ9) or provide your own `public static void main(String[] args)` that calls"
              + " `new MyScript().bootstrap(args)`.");
    }
    String mainClassName = command.split("\\s+", 2)[0];
    Class<?> clazz = Class.forName(mainClassName);
    if (!expected.isAssignableFrom(clazz)) {
      throw new IllegalStateException(
          mainClassName + " is not a subclass of " + expected.getName());
    }
    try {
      return expected.cast(clazz.getDeclaredConstructor().newInstance());
    } catch (NoSuchMethodException nsme) {
      throw new IllegalStateException(
          mainClassName
              + " must have a public no-argument constructor to be instantiated by bleepscript",
          nsme);
    }
  }
}
