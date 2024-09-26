package bleep.logging

import sourcecode.{Enclosing, File, Line}

import java.time.Instant

final class Metadata(
    val instant: Instant,
    val logLevel: LogLevel,
    val line: Line,
    val file: File,
    val enclosing: Enclosing
)
