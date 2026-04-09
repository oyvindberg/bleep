package bleep.testing

/** Detects and collapses repeating frame cycles in stack traces.
  *
  * StackOverflowError produces thousands of identical frame sequences. This utility detects the repeating pattern and collapses it to a single occurrence with
  * a repetition count.
  *
  * Handles:
  *   - Repeating cycles of "at ..." frames
  *   - "Caused by:" sections (each processed independently)
  *   - JVM's own "... N more" lines (preserved as-is)
  *   - Partial cycles at boundaries
  */
object StackTraceCycles {

  /** Collapse repeating cycles in a stack trace string. Returns the collapsed lines. */
  def collapse(stackTrace: String): List[String] = {
    val lines = stackTrace.split("\n").toList
    collapseSegments(lines)
  }

  /** Split lines into segments separated by "Caused by:" headers, collapse each frame block independently, then reassemble.
    */
  private def collapseSegments(lines: List[String]): List[String] = {
    // Split into segments: each segment starts with a non-frame line (exception header or "Caused by:")
    // and is followed by frame lines ("at ..." or "... N more")
    val result = List.newBuilder[String]
    var currentFrames = List.newBuilder[String]
    var hasFrames = false

    lines.foreach { line =>
      val trimmed = line.trim
      if (isFrameLine(trimmed)) {
        currentFrames += line
        hasFrames = true
      } else {
        // Non-frame line — flush any accumulated frames first
        if (hasFrames) {
          result ++= collapseFrameCycles(currentFrames.result())
          currentFrames = List.newBuilder[String]
          hasFrames = false
        }
        result += line
      }
    }

    // Flush remaining frames
    if (hasFrames) {
      result ++= collapseFrameCycles(currentFrames.result())
    }

    result.result()
  }

  /** A frame line is "at ...", "\tat ...", or "... N more" */
  private def isFrameLine(trimmed: String): Boolean =
    trimmed.startsWith("at ") || trimmed.startsWith("... ")

  /** Given a block of consecutive frame lines, detect and collapse repeating cycles.
    *
    * Algorithm: try cycle lengths from 2 up to half the block size. For each candidate length, check if the first N lines repeat consecutively. Take the
    * shortest cycle that repeats at least 3 times.
    */
  private def collapseFrameCycles(frames: List[String]): List[String] = {
    val n = frames.size
    if (n < 6) return frames // Need at least 3 repetitions of length 2

    val arr = frames.toArray

    // Try cycle lengths from 1 to n/3 (need at least 3 reps to be worth collapsing)
    val maxCycleLen = n / 3
    var bestCycleLen = -1
    var bestReps = 0
    var bestOffset = 0

    // Try starting at offset 0 first (most common case for StackOverflowError)
    var cycleLen = 1
    while (cycleLen <= maxCycleLen && bestCycleLen == -1) {
      val reps = countRepetitions(arr, 0, cycleLen)
      if (reps >= 3) {
        bestCycleLen = cycleLen
        bestReps = reps
        bestOffset = 0
      }
      cycleLen += 1
    }

    // If no cycle found starting at 0, try skipping a few leading frames
    if (bestCycleLen == -1) {
      var offset = 1
      while (offset < math.min(10, n / 3) && bestCycleLen == -1) {
        val remaining = n - offset
        val maxCL = remaining / 3
        cycleLen = 1
        while (cycleLen <= maxCL && bestCycleLen == -1) {
          val reps = countRepetitions(arr, offset, cycleLen)
          if (reps >= 3) {
            bestCycleLen = cycleLen
            bestReps = reps
            bestOffset = offset
          }
          cycleLen += 1
        }
        offset += 1
      }
    }

    if (bestCycleLen == -1) return frames // No cycle found

    // Build result: prefix + one cycle + indicator + suffix
    val result = List.newBuilder[String]

    // Lines before the cycle
    var i = 0
    while (i < bestOffset) {
      result += arr(i)
      i += 1
    }

    // One instance of the cycle
    i = bestOffset
    val cycleEnd = bestOffset + bestCycleLen
    while (i < cycleEnd) {
      result += arr(i)
      i += 1
    }

    // Cycle indicator — use same indentation as the frame lines
    val indent = detectIndent(arr(bestOffset))
    result += s"$indent... above $bestCycleLen frames repeated $bestReps times (${bestCycleLen * bestReps} frames total)"

    // Lines after the cycle
    val afterCycle = bestOffset + bestCycleLen * bestReps
    i = afterCycle
    while (i < n) {
      result += arr(i)
      i += 1
    }

    result.result()
  }

  /** Count how many times the sequence arr[offset..offset+cycleLen) repeats consecutively starting at offset. */
  private def countRepetitions(arr: Array[String], offset: Int, cycleLen: Int): Int = {
    val n = arr.length
    var reps = 1
    var pos = offset + cycleLen
    var matching = true
    while (matching && pos + cycleLen <= n) {
      var j = 0
      while (j < cycleLen && matching) {
        if (arr(offset + j).trim != arr(pos + j).trim) {
          matching = false
        }
        j += 1
      }
      if (matching) {
        reps += 1
        pos += cycleLen
      }
    }
    reps
  }

  /** Detect leading whitespace from a frame line. */
  private def detectIndent(line: String): String = {
    val idx = line.indexWhere(!_.isWhitespace)
    if (idx > 0) line.substring(0, idx) else "\t"
  }
}
