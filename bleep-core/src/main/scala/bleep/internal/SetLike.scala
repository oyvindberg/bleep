package bleep.internal

trait SetLike[T <: SetLike[T]] extends Any {
  def intersect(other: T): T
  def removeAll(other: T): T
  def union(other: T): T
  def isEmpty: Boolean

  final def removeAllDropEmpty(other: T): Option[T] = Some(this.removeAll(other)).filterNot(_.isEmpty)
  final def intersectDropEmpty(other: T): Option[T] = Some(this.intersect(other)).filterNot(_.isEmpty)
}
