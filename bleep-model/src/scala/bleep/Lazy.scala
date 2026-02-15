package bleep

import scala.util.control.NonFatal

// Guard against circular evaluation.
// Thread-safe: concurrent calls will block until computation completes.
// Returns None only for true circular dependencies (same thread re-entering).
sealed trait Lazy[T] {
  @volatile protected var state: Lazy.State[T] = Lazy.State.Initial

  def get: Option[T]

  final def forceGet: T =
    get.getOrElse(sys.error(s"Unexpected circular"))

  final def forceGet(circumstance: String): T =
    get.getOrElse(sys.error(s"Unexpected circular: $circumstance"))

  final def map[U](f: T => U): Lazy[U] =
    new Lazy.Mapped(this, f)

  final def getIfEvaluated: Option[T] =
    state match {
      case Lazy.State.Initial | Lazy.State.Computing(_) => None
      case Lazy.State.Done(value)                       => Some(value)
      case Lazy.State.Failed(th)                        => throw th
    }
}

object Lazy {
  def apply[T](compute: => T): Lazy[T] = new Lazy[T] {
    private val lock = new Object

    override def get: Option[T] = {
      val currentThread = Thread.currentThread()

      // Fast path: check if already computed without acquiring lock
      state match {
        case State.Done(value) => return Some(value)
        case State.Failed(th)  => throw th
        case _                 => ()
      }

      lock.synchronized {
        // Re-check state after acquiring lock
        var done = false
        var result: Option[T] = None

        while (!done)
          state match {
            case State.Initial =>
              state = State.Computing(currentThread)
              try {
                val computed = compute
                state = State.Done(computed)
                result = Some(computed)
                done = true
                lock.notifyAll() // Wake up any waiting threads
              } catch {
                case NonFatal(th) =>
                  state = State.Failed(th)
                  lock.notifyAll()
                  throw th
              }

            case State.Computing(computingThread) =>
              if (computingThread == currentThread) {
                // Same thread re-entering - true circular dependency
                result = None
                done = true
              } else {
                // Different thread - wait for computation to complete
                lock.wait()
              }

            case State.Done(value) =>
              result = Some(value)
              done = true

            case State.Failed(th) =>
              throw th
          }

        result
      }
    }
  }

  private final class Mapped[T, U](outer: Lazy[T], f: T => U) extends Lazy[U] {
    private val lock = new Object

    override def get: Option[U] = {
      val currentThread = Thread.currentThread()

      // Fast path: check if already computed without acquiring lock
      state match {
        case State.Done(value) => return Some(value)
        case State.Failed(th)  => throw th
        case _                 => ()
      }

      lock.synchronized {
        var done = false
        var result: Option[U] = None

        while (!done)
          state match {
            case State.Initial =>
              outer.get match {
                case None =>
                  // Outer returned None (circular in outer)
                  result = None
                  done = true
                case Some(gotten) =>
                  state = State.Computing(currentThread)
                  try {
                    val computed = f(gotten)
                    state = State.Done(computed)
                    result = Some(computed)
                    done = true
                    lock.notifyAll()
                  } catch {
                    case NonFatal(th) =>
                      state = State.Failed(th)
                      lock.notifyAll()
                      throw th
                  }
              }

            case State.Computing(computingThread) =>
              if (computingThread == currentThread) {
                // Same thread re-entering - true circular dependency
                result = None
                done = true
              } else {
                // Different thread - wait for computation to complete
                lock.wait()
              }

            case State.Done(value) =>
              result = Some(value)
              done = true

            case State.Failed(th) =>
              throw th
          }

        result
      }
    }
  }

  sealed trait State[+T]
  object State {
    case object Initial extends State[Nothing]
    case class Computing(thread: Thread) extends State[Nothing]
    case class Done[T](value: T) extends State[T]
    case class Failed(th: Throwable) extends State[Nothing]
  }
}
