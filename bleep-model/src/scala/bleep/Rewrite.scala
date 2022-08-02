package bleep

import bleep.model.ExplodedBuild

trait Rewrite {
  val name: String

  def apply(explodedBuild: ExplodedBuild): ExplodedBuild
}
