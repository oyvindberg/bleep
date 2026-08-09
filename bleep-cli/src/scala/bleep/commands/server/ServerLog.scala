package bleep
package commands
package server

import bleep.bsp.{BspServerOperations, ServerDirInfo, ServerDirs}
import ryddig.Logger

import java.nio.file.{Files, Path}

/** `bleep server log` — the daemon's own log.
  *
  * Read from `socketDir/output` on disk rather than streamed over the protocol, deliberately. Tailing a file works on a daemon that has crashed or been killed
  * — which is when you actually want the log — works across daemon versions, and needs nothing from the server side. A protocol log stream would add a
  * broadcast channel the daemon does not have, and it would go quiet at exactly the wrong moment.
  *
  * `--generation` reaches the rotated files. Rotation happens only at spawn, so `output.1` is the previous daemon's log — usually the one holding the crash.
  */
case class ServerLog(
    logger: Logger,
    userPaths: UserPaths,
    id: Option[String],
    lines: Int,
    follow: Boolean,
    generation: Int
) extends BleepCommand {

  override def run(): Either[BleepException, Unit] = {
    val infos = ServerDirs.scan(userPaths)

    select(infos).flatMap { info =>
      val file = logFile(info.socketDir)
      if (!Files.exists(file)) Left(new BleepException.Text(s"no log at $file"))
      else {
        printTail(file)
        if (follow) followFile(file)
        Right(())
      }
    }
  }

  private def logFile(socketDir: Path): Path =
    socketDir.resolve(if (generation == 0) "output" else s"output.$generation")

  private def select(infos: List[ServerDirInfo]): Either[BleepException, ServerDirInfo] =
    id match {
      case Some(wanted) => ServerDirs.resolve(infos, wanted).left.map(msg => new BleepException.Text(msg))
      case None         =>
        // Prefer a running server, but fall back to a single dead one: reading a dead daemon's log is a main reason to be here.
        infos.filter(_.isRunning) match {
          case one :: Nil => Right(one)
          case Nil        =>
            infos match {
              case one :: Nil => Right(one)
              case Nil        => Left(new BleepException.Text("no compile servers"))
              case many       => Left(new BleepException.Text(s"pick one: ${many.map(_.hash).mkString(", ")}"))
            }
          case many => Left(new BleepException.Text(s"${many.size} servers are running — pick one: ${many.map(_.hash).mkString(", ")}"))
        }
    }

  private def printTail(file: Path): Unit =
    BspServerOperations.readLogFile(file).linesIterator.toList.takeRight(lines).foreach(println)

  /** Poll for growth. Reopen when the file shrinks or is replaced — rotation only happens while the daemon is down, but a `kill` followed by a fresh spawn is a
    * perfectly ordinary thing to do while watching.
    */
  private def followFile(file: Path): Unit = {
    var position = Files.size(file)
    while (true) {
      Thread.sleep(300)
      if (Files.exists(file)) {
        val size = Files.size(file)
        if (size < position) {
          logger.info(s"-- $file was truncated or replaced, following from the start --")
          position = 0
        }
        if (size > position) {
          val channel = Files.newByteChannel(file)
          try {
            channel.position(position)
            val buffer = java.nio.ByteBuffer.allocate((size - position).toInt)
            channel.read(buffer): Unit
            print(new String(buffer.array(), java.nio.charset.StandardCharsets.UTF_8))
            Console.out.flush()
          } finally channel.close()
          position = size
        }
      }
    }
  }
}
