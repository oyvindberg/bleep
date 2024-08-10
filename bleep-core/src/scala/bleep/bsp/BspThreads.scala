package bleep.bsp

import bloop.rifle.BloopThreads

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ExecutorService, Executors, ThreadFactory}

final case class BspThreads(
    buildThreads: BloopThreads,
    prepareBuildExecutor: ExecutorService
) {
  def shutdown(): Unit = {
    buildThreads.shutdown()
    prepareBuildExecutor.shutdown()
  }
}

object BspThreads {
  def withThreads[T](f: BspThreads => T): T = {
    var threads: BspThreads = null
    try {
      threads = create()
      f(threads)
    } finally if (threads != null) threads.shutdown()
  }
  def create(): BspThreads =
    BspThreads(
      BloopThreads.create(),
      Executors.newSingleThreadExecutor(
        daemonThreadFactory("scala-cli-bsp-prepare-build-thread")
      )
    )

  private def daemonThreadFactory(prefix: String): ThreadFactory =
    new ThreadFactory {
      val counter = new AtomicInteger
      def threadNumber() = counter.incrementAndGet()
      def newThread(r: Runnable) =
        new Thread(r, s"$prefix-thread-${threadNumber()}") {
          setDaemon(true)
          setPriority(Thread.NORM_PRIORITY)
        }
    }
}
