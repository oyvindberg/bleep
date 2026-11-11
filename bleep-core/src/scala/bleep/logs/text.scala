package bleep.logs

/** Truncate for display, marking that something was cut. */
def abbreviate(s: String, maxChars: Int): String =
  if (s.length <= maxChars) s else s.take(maxChars - 1) + "…"

/** A recorded line as an agent or a plain-text reader should see it: colour codes are kept in the log on disk and removed when it is served. */
def stripAnsi(s: String): String = bleep.history.TranscriptFormat.stripAnsi(s)
