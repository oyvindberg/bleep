package bleep

package object model {
  val $schema = "https://raw.githubusercontent.com/oyvindberg/bleep/master/schema.json"

  private[bleep] def removeAllFrom[T <: SetLike[T]](ot1: Option[T], ot2: Option[T]): Option[T] =
    ot2 match {
      case Some(t2) => ot1.map(t1 => t1.removeAll(t2))
      case None     => ot1
    }
}
