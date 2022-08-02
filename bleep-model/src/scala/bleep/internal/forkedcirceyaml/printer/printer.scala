package bleep.internal.forkedcirceyaml

import io.circe.Json

package object printer {

  /** A default printer implementation using Snake YAML.
    */
  def print(tree: Json): String = Printer.spaces2.pretty(tree)
}
