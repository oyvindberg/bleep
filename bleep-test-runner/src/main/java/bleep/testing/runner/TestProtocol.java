package bleep.testing.runner;

import java.util.ArrayList;
import java.util.List;

/**
 * Simple JSON-based protocol for communication between bleep and forked test JVMs.
 *
 * <p>Outputs valid JSON that can be parsed by circe on the Scala side. Each message is a single
 * line of JSON.
 */
public final class TestProtocol {

  private TestProtocol() {}

  // === Commands (parent -> forked JVM) ===

  public static final String CMD_RUN_SUITE = "RunSuite";
  public static final String CMD_SHUTDOWN = "Shutdown";
  public static final String CMD_GET_THREAD_DUMP = "GetThreadDump";

  // === Responses (forked JVM -> parent) ===

  public static final String RESP_READY = "Ready";
  public static final String RESP_TEST_STARTED = "TestStarted";
  public static final String RESP_TEST_FINISHED = "TestFinished";
  public static final String RESP_SUITE_DONE = "SuiteDone";
  public static final String RESP_LOG = "Log";
  public static final String RESP_ERROR = "Error";
  public static final String RESP_THREAD_DUMP = "ThreadDump";

  // === Response encoding (forked JVM outputs these) ===

  public static String encodeReady() {
    return "{\"type\":\"Ready\"}";
  }

  public static String encodeTestStarted(String suite, String test) {
    return "{\"type\":\"TestStarted\",\"data\":{\"suite\":"
        + jsonString(suite)
        + ",\"test\":"
        + jsonString(test)
        + "}}";
  }

  public static String encodeTestFinished(
      String suite, String test, String status, long durationMs, String message, String throwable) {
    StringBuilder sb = new StringBuilder();
    sb.append("{\"type\":\"TestFinished\",\"data\":{");
    sb.append("\"suite\":").append(jsonString(suite));
    sb.append(",\"test\":").append(jsonString(test));
    sb.append(",\"status\":").append(jsonString(status));
    sb.append(",\"durationMs\":").append(durationMs);
    if (message != null) {
      sb.append(",\"message\":").append(jsonString(message));
    }
    if (throwable != null) {
      sb.append(",\"throwable\":").append(jsonString(throwable));
    }
    sb.append("}}");
    return sb.toString();
  }

  public static String encodeSuiteDone(
      String suite, int passed, int failed, int skipped, int ignored, long durationMs) {
    return "{\"type\":\"SuiteDone\",\"data\":{"
        + "\"suite\":"
        + jsonString(suite)
        + ",\"passed\":"
        + passed
        + ",\"failed\":"
        + failed
        + ",\"skipped\":"
        + skipped
        + ",\"ignored\":"
        + ignored
        + ",\"durationMs\":"
        + durationMs
        + "}}";
  }

  public static String encodeLog(String level, String message) {
    return "{\"type\":\"Log\",\"data\":{\"level\":"
        + jsonString(level)
        + ",\"message\":"
        + jsonString(message)
        + "}}";
  }

  public static String encodeError(String message, String throwable) {
    StringBuilder sb = new StringBuilder();
    sb.append("{\"type\":\"Error\",\"data\":{");
    sb.append("\"message\":").append(jsonString(message));
    if (throwable != null) {
      sb.append(",\"throwable\":").append(jsonString(throwable));
    }
    sb.append("}}");
    return sb.toString();
  }

  /** Encode thread dump information as a list of threads with their stack traces */
  public static String encodeThreadDump(List<ThreadDumpEntry> threads) {
    StringBuilder sb = new StringBuilder();
    sb.append("{\"type\":\"ThreadDump\",\"data\":{\"threads\":[");
    boolean first = true;
    for (ThreadDumpEntry entry : threads) {
      if (!first) sb.append(",");
      first = false;
      sb.append("{\"name\":").append(jsonString(entry.name));
      sb.append(",\"state\":").append(jsonString(entry.state));
      sb.append(",\"stackTrace\":[");
      boolean firstLine = true;
      for (String line : entry.stackTrace) {
        if (!firstLine) sb.append(",");
        firstLine = false;
        sb.append(jsonString(line));
      }
      sb.append("]}");
    }
    sb.append("]}}");
    return sb.toString();
  }

  /** Thread dump entry for encoding */
  public static class ThreadDumpEntry {
    public final String name;
    public final String state;
    public final List<String> stackTrace;

    public ThreadDumpEntry(String name, String state, List<String> stackTrace) {
      this.name = name;
      this.state = state;
      this.stackTrace = stackTrace;
    }
  }

  // === Command parsing (forked JVM receives these) ===

  /**
   * Parse a command from a JSON line. Format:
   * {"type":"RunSuite","data":{"className":"...","framework":"...","args":[...]}} or
   * {"type":"Shutdown"}
   */
  public static ParsedCommand parseCommand(String line) {
    if (line == null || line.isEmpty()) {
      return new ParsedCommand.Invalid("Empty command");
    }

    try {
      // Find the type field
      String type = extractStringField(line, "type");
      if (type == null) {
        return new ParsedCommand.Invalid("Missing type field");
      }

      if (CMD_SHUTDOWN.equals(type)) {
        return new ParsedCommand.Shutdown();
      } else if (CMD_GET_THREAD_DUMP.equals(type)) {
        return new ParsedCommand.GetThreadDump();
      } else if (CMD_RUN_SUITE.equals(type)) {
        // Extract data object fields
        int dataStart = line.indexOf("\"data\"");
        if (dataStart < 0) {
          return new ParsedCommand.Invalid("Missing data field for RunSuite");
        }
        String dataSection = line.substring(dataStart);

        String className = extractStringField(dataSection, "className");
        String framework = extractStringField(dataSection, "framework");
        List<String> args = extractStringArray(dataSection, "args");

        if (className == null || framework == null) {
          return new ParsedCommand.Invalid("Missing className or framework");
        }

        return new ParsedCommand.RunSuite(
            className, framework, args != null ? args : new ArrayList<String>());
      } else {
        return new ParsedCommand.Invalid("Unknown command type: " + type);
      }
    } catch (Exception e) {
      return new ParsedCommand.Invalid("Parse error: " + e.getMessage());
    }
  }

  // === Simple JSON utilities ===

  /** Escape and quote a string for JSON output. */
  private static String jsonString(String s) {
    if (s == null) return "null";
    StringBuilder sb = new StringBuilder(s.length() + 2);
    sb.append('"');
    for (int i = 0; i < s.length(); i++) {
      char c = s.charAt(i);
      switch (c) {
        case '"':
          sb.append("\\\"");
          break;
        case '\\':
          sb.append("\\\\");
          break;
        case '\b':
          sb.append("\\b");
          break;
        case '\f':
          sb.append("\\f");
          break;
        case '\n':
          sb.append("\\n");
          break;
        case '\r':
          sb.append("\\r");
          break;
        case '\t':
          sb.append("\\t");
          break;
        default:
          if (c < 0x20) {
            sb.append(String.format("\\u%04x", (int) c));
          } else {
            sb.append(c);
          }
      }
    }
    sb.append('"');
    return sb.toString();
  }

  /** Extract a string field value from JSON (simple parsing). */
  private static String extractStringField(String json, String fieldName) {
    String pattern = "\"" + fieldName + "\"";
    int idx = json.indexOf(pattern);
    if (idx < 0) return null;

    idx += pattern.length();
    // Skip whitespace and colon
    while (idx < json.length() && (json.charAt(idx) == ' ' || json.charAt(idx) == ':')) {
      idx++;
    }
    if (idx >= json.length() || json.charAt(idx) != '"') return null;

    idx++; // Skip opening quote
    StringBuilder sb = new StringBuilder();
    while (idx < json.length()) {
      char c = json.charAt(idx);
      if (c == '"') break;
      if (c == '\\' && idx + 1 < json.length()) {
        idx++;
        char escaped = json.charAt(idx);
        switch (escaped) {
          case '"':
            sb.append('"');
            break;
          case '\\':
            sb.append('\\');
            break;
          case 'n':
            sb.append('\n');
            break;
          case 'r':
            sb.append('\r');
            break;
          case 't':
            sb.append('\t');
            break;
          case 'b':
            sb.append('\b');
            break;
          case 'f':
            sb.append('\f');
            break;
          case 'u':
            if (idx + 4 < json.length()) {
              String hex = json.substring(idx + 1, idx + 5);
              sb.append((char) Integer.parseInt(hex, 16));
              idx += 4;
            }
            break;
          default:
            sb.append(escaped);
        }
      } else {
        sb.append(c);
      }
      idx++;
    }
    return sb.toString();
  }

  /** Extract a string array field value from JSON (simple parsing). */
  private static List<String> extractStringArray(String json, String fieldName) {
    String pattern = "\"" + fieldName + "\"";
    int idx = json.indexOf(pattern);
    if (idx < 0) return null;

    idx += pattern.length();
    // Skip to opening bracket
    while (idx < json.length() && json.charAt(idx) != '[') {
      idx++;
    }
    if (idx >= json.length()) return null;
    idx++; // Skip [

    List<String> result = new ArrayList<>();
    StringBuilder current = null;
    boolean inString = false;
    boolean escape = false;

    while (idx < json.length()) {
      char c = json.charAt(idx);

      if (escape) {
        if (current != null) {
          switch (c) {
            case '"':
              current.append('"');
              break;
            case '\\':
              current.append('\\');
              break;
            case 'n':
              current.append('\n');
              break;
            case 'r':
              current.append('\r');
              break;
            case 't':
              current.append('\t');
              break;
            default:
              current.append(c);
          }
        }
        escape = false;
      } else if (c == '\\' && inString) {
        escape = true;
      } else if (c == '"') {
        if (inString) {
          result.add(current.toString());
          current = null;
        } else {
          current = new StringBuilder();
        }
        inString = !inString;
      } else if (c == ']' && !inString) {
        break;
      } else if (inString && current != null) {
        current.append(c);
      }
      idx++;
    }

    return result;
  }

  // === Command encoding (parent sends these) ===

  public static String encodeRunSuite(String className, String framework, List<String> args) {
    StringBuilder sb = new StringBuilder();
    sb.append("{\"type\":\"RunSuite\",\"data\":{");
    sb.append("\"className\":").append(jsonString(className));
    sb.append(",\"framework\":").append(jsonString(framework));
    sb.append(",\"args\":[");
    for (int i = 0; i < args.size(); i++) {
      if (i > 0) sb.append(",");
      sb.append(jsonString(args.get(i)));
    }
    sb.append("]}}");
    return sb.toString();
  }

  public static String encodeShutdown() {
    return "{\"type\":\"Shutdown\"}";
  }

  // === Parsed command types ===

  public abstract static class ParsedCommand {
    public static final class RunSuite extends ParsedCommand {
      public final String className;
      public final String framework;
      public final List<String> args;

      public RunSuite(String className, String framework, List<String> args) {
        this.className = className;
        this.framework = framework;
        this.args = args;
      }
    }

    public static final class Shutdown extends ParsedCommand {}

    public static final class GetThreadDump extends ParsedCommand {}

    public static final class Invalid extends ParsedCommand {
      public final String message;

      public Invalid(String message) {
        this.message = message;
      }
    }
  }
}
