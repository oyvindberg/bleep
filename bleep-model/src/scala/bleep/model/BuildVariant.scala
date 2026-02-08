package bleep.model

sealed trait BuildVariant {
  def name: String
}

object BuildVariant {
  case object Normal extends BuildVariant {
    val name = "normal"
  }
  case object BSP extends BuildVariant {
    val name = "bsp"
  }

  /** Simplified: store the variant name as a string (directory-safe) */
  case class Rewritten(name: String) extends BuildVariant

  /** Factory for creating from rewrite names (used by bootstrap.scala) */
  def apply(rewrites: List[BuildRewriteName]): BuildVariant =
    if (rewrites.isEmpty) Normal
    else Rewritten(rewrites.map(_.value).mkString("__"))

  /** Factory for parsing from string (used by BSP server) */
  def fromName(name: String): BuildVariant = name match {
    case "normal" => Normal
    case "bsp"    => BSP
    case other    => Rewritten(other)
  }
}
