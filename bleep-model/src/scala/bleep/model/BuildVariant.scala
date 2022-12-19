package bleep.model

import cats.data.NonEmptyList

sealed trait BuildVariant

object BuildVariant {
  case object Normal extends BuildVariant
  case object BSP extends BuildVariant

  case class Rewritten(rewrites: NonEmptyList[BuildRewriteName]) extends BuildVariant

  def apply(rewrites: List[BuildRewriteName]): BuildVariant =
    NonEmptyList.fromList(rewrites) match {
      case Some(nonEmpty) => Rewritten(nonEmpty)
      case None           => Normal
    }
}
