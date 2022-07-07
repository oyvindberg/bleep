package bleep

trait Rewrite {
  val name: String

  def apply(explodedBuild: ExplodedBuild): ExplodedBuild
}
