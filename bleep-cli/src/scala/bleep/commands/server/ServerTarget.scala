package bleep
package commands
package server

import bleep.bsp.{ServerDirInfo, ServerDirs}

import java.nio.file.Path

/** How every `bleep server` subcommand decides which daemon you meant.
  *
  * One rule, applied the same way by `status`, `log` and `kill`, because a machine with four daemons on it is normal and having each command guess differently
  * would be its own kind of hostile:
  *
  *   1. an id you named — a pid, a socket-dir hash, or an unambiguous prefix
  *   1. the daemon serving the build you are standing in — the one `ls` marks "← this build"
  *   1. the only one running, if there is exactly one
  *   1. otherwise, refuse and list the candidates rather than picking for you
  */
object ServerTarget {

  def select(
      infos: List[ServerDirInfo],
      id: Option[String],
      currentWorkspace: Option[Path],
      allowStopped: Boolean,
      what: String
  ): Either[BleepException, ServerDirInfo] =
    id match {
      case Some(wanted) => ServerDirs.resolve(infos, wanted).left.map(msg => new BleepException.Text(msg))
      case None         =>
        currentWorkspace.flatMap(workspace => ServerDirs.servingWorkspace(infos, workspace)) match {
          case Some(mine) => Right(mine)
          case None       =>
            infos.filter(_.isRunning) match {
              case one :: Nil          => Right(one)
              case Nil if allowStopped =>
                infos match {
                  case one :: Nil => Right(one)
                  case Nil        => Left(new BleepException.Text("no compile servers"))
                  case many       => Left(new BleepException.Text(s"no server is running — name one to $what: ${many.map(_.hash).mkString(", ")}"))
                }
              case Nil  => Left(new BleepException.Text("no compile server is running. Run 'bleep server ls' to see stopped ones."))
              case many =>
                Left(
                  new BleepException.Text(
                    s"${many.size} servers are running and none serves this build — name one to $what: ${many.map(_.hash).mkString(", ")}"
                  )
                )
            }
        }
    }
}
