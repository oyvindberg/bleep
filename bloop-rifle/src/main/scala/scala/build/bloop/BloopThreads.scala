package scala.build.bloop

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ExecutorService, Executors, ScheduledExecutorService, ThreadFactory}

final case class BloopThreads(
    jsonrpc: ExecutorService,
    startServerChecks: ScheduledExecutorService
) {
  def shutdown(): Unit =
    jsonrpc.shutdown()
    //startServerChecks.shutdown()
}

object BloopThreads {
  def create(): BloopThreads = {
    val jsonrpc = Executors.newFixedThreadPool(4, daemonThreadFactory("bleep-bsp-jsonrpc"))
    val startServerChecks = Executors.newSingleThreadScheduledExecutor(daemonThreadFactory("bleep-bloop-rifle"))
    BloopThreads(jsonrpc, startServerChecks)
  }

  def daemonThreadFactory(prefix: String): ThreadFactory =
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
