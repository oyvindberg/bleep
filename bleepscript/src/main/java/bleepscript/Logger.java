package bleepscript;

public interface Logger {
  void debug(String msg);

  void debug(String msg, Throwable t);

  void info(String msg);

  void info(String msg, Throwable t);

  void warn(String msg);

  void warn(String msg, Throwable t);

  void error(String msg);

  void error(String msg, Throwable t);

  Logger withContext(String key, String value);

  Logger withContext(String key, Object value);

  Logger withPath(String fragment);
}
