package bleep

trait Rewrite {
  val name: String

  def apply(explodedBuild: model.ExplodedBuild): model.ExplodedBuild
}
