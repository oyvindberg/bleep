package bleep.bsp

import java.io.{IOException, InputStream, OutputStream}
import java.util.concurrent.locks.ReentrantLock

/** Moves bytes from one thread to another inside one JVM.
  *
  * `java.io.PipedInputStream` records the thread that wrote most recently. Its `read` throws `Write end dead` once that thread has exited and the buffer has
  * run empty, even while other threads keep writing. Cats Effect retires the carrier thread of a blocking region. A BSP notification sent from such a region
  * leaves a retired thread recorded. This pipe records no thread. A read blocks until a write arrives or until [[sink]] closes.
  *
  * @param capacity
  *   how many bytes the pipe keeps before a write blocks
  */
final class InMemoryPipe(capacity: Int) {

  private val bytes = new Array[Byte](capacity)
  private val lock = new ReentrantLock()
  private val notEmpty = lock.newCondition()
  private val notFull = lock.newCondition()

  private var head = 0
  private var count = 0
  private var sinkClosed = false
  private var sourceClosed = false

  /** Reads what [[sink]] wrote. A read returns -1 once [[sink]] has closed and the buffer has run empty. */
  val source: InputStream = new InputStream {

    override def read(): Int = {
      val one = new Array[Byte](1)
      if (read(one, 0, 1) < 0) -1 else one(0) & 0xff
    }

    override def read(destination: Array[Byte], offset: Int, length: Int): Int =
      if (length == 0) 0
      else {
        lock.lock()
        try {
          while (count == 0 && !sinkClosed && !sourceClosed) notEmpty.await()
          if (count == 0) -1
          else {
            val taken = math.min(length, count)
            val toEnd = math.min(taken, capacity - head)
            System.arraycopy(bytes, head, destination, offset, toEnd)
            System.arraycopy(bytes, 0, destination, offset + toEnd, taken - toEnd)
            head = (head + taken) % capacity
            count -= taken
            notFull.signalAll()
            taken
          }
        } finally lock.unlock()
      }

    override def available(): Int = {
      lock.lock()
      try count
      finally lock.unlock()
    }

    override def close(): Unit = {
      lock.lock()
      try {
        sourceClosed = true
        notEmpty.signalAll()
        notFull.signalAll()
      } finally lock.unlock()
    }
  }

  /** Writes bytes for [[source]] to read. A write blocks while the pipe is full. Every write is visible to [[source]] before the call returns, which leaves
    * `flush` nothing to do.
    */
  val sink: OutputStream = new OutputStream {

    override def write(byte: Int): Unit = {
      val one = new Array[Byte](1)
      one(0) = byte.toByte
      write(one, 0, 1)
    }

    /** @throws PipeClosedByReaderException
      *   when [[source]] is closed
      * @throws PipeClosedByWriterException
      *   when this sink is closed
      */
    override def write(written: Array[Byte], offset: Int, length: Int): Unit = {
      lock.lock()
      try {
        var done = 0
        while (done < length) {
          while (count == capacity && !sourceClosed && !sinkClosed) notFull.await()
          if (sourceClosed) throw PipeClosedByReaderException()
          if (sinkClosed) throw PipeClosedByWriterException()
          val room = math.min(length - done, capacity - count)
          val tail = (head + count) % capacity
          val toEnd = math.min(room, capacity - tail)
          System.arraycopy(written, offset + done, bytes, tail, toEnd)
          System.arraycopy(written, offset + done + toEnd, bytes, 0, room - toEnd)
          count += room
          done += room
          notEmpty.signalAll()
        }
      } finally lock.unlock()
    }

    override def flush(): Unit = ()

    override def close(): Unit = {
      lock.lock()
      try {
        sinkClosed = true
        notEmpty.signalAll()
        notFull.signalAll()
      } finally lock.unlock()
    }
  }
}

/** Thrown when a write arrives after the reading end closed. The bytes of that write reach no reader. */
case class PipeClosedByReaderException() extends IOException("The reading end of this pipe is closed")

/** Thrown when a write arrives after the writing end closed. */
case class PipeClosedByWriterException() extends IOException("The writing end of this pipe is closed")
