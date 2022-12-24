package bleep
package internal

object traverseish {
  def runAll[T](all: Iterable[T])(f: T => Either[BleepException, Unit]): Either[BleepException, Unit] = {
    val it = all.iterator
    while (it.hasNext) {
      val t = it.next()
      f(t) match {
        case left @ Left(_) => return left
        case Right(())      => ()
      }
    }
    Right(())
  }
}
