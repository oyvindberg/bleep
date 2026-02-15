package bleep.analysis

import java.nio.file.Path

/** Language of a source file */
enum Language {
  case Java, Kotlin, Scala

  def extensions: Set[String] = this match {
    case Java   => Set(".java")
    case Kotlin => Set(".kt", ".kts")
    case Scala  => Set(".scala")
  }
}

object Language {
  def fromPath(path: Path): Option[Language] = {
    val name = path.getFileName.toString.toLowerCase
    Language.values.find(_.extensions.exists(name.endsWith))
  }
}

/** Structured parse/analysis error for a single location */
case class ParseError(
    path: Path,
    line: Int, // 1-based line number, 0 if unknown
    column: Int, // 1-based column number, 0 if unknown
    message: String,
    severity: ParseError.Severity = ParseError.Severity.Error
) {
  def formatted: String = {
    val loc = (line, column) match {
      case (0, 0) => ""
      case (l, 0) => s":$l"
      case (l, c) => s":$l:$c"
    }
    s"${path.getFileName}$loc: $message"
  }
}

object ParseError {
  enum Severity {
    case Error, Warning
  }

  /** Create from a simple message (unknown location) */
  def simple(path: Path, message: String): ParseError =
    ParseError(path, 0, 0, message)

  /** Parse location from "line:column: message" format */
  def fromString(path: Path, s: String): ParseError = {
    val pattern = """^(\d+):(\d+):\s*(.*)$""".r
    val lineOnlyPattern = """^(\d+):\s*(.*)$""".r
    s match {
      case pattern(line, col, msg) =>
        ParseError(path, line.toInt, col.toInt, msg.trim)
      case lineOnlyPattern(line, msg) =>
        ParseError(path, line.toInt, 0, msg.trim)
      case _ =>
        ParseError(path, 0, 0, s)
    }
  }
}

/** Collection of parse errors for multiple files */
case class ParseErrors(errors: List[ParseError]) {
  def isEmpty: Boolean = errors.isEmpty
  def nonEmpty: Boolean = errors.nonEmpty

  def byFile: Map[Path, List[ParseError]] =
    errors.groupBy(_.path)

  def formatted: String =
    errors.map(_.formatted).mkString("\n")

  def ++(other: ParseErrors): ParseErrors =
    ParseErrors(errors ++ other.errors)
}

object ParseErrors {
  val empty: ParseErrors = ParseErrors(Nil)

  def apply(error: ParseError): ParseErrors = ParseErrors(List(error))

  def fromMap(errors: Map[Path, String]): ParseErrors = {
    val allErrors = errors.toList.flatMap { case (path, msg) =>
      msg.split("\n").toList.map(line => ParseError.fromString(path, line))
    }
    ParseErrors(allErrors)
  }
}
