package bleep.internal.forkedcirceyaml

import io.circe.Json

/** Provides helpful syntax that is not specific to the SnakeYAML implementation.
  */
object syntax {

  final class YamlSyntax(val tree: Json) extends AnyVal {
    def spaces2: String = Printer.spaces2.pretty(tree)
    def spaces4: String = Printer.spaces4.pretty(tree)
  }

  /** Call this to serialize a [[Json]] AST into a YAML string using the default options.
    */
  implicit class AsYaml(val tree: Json) {
    def asYaml: YamlSyntax = new YamlSyntax(tree)
  }
}
