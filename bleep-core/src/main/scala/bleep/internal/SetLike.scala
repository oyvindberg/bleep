package bleep.internal

trait SetLike[T] extends Any {
  def intersect(other: T): T
  def removeAll(other: T): T
  def union(other: T): T
}
