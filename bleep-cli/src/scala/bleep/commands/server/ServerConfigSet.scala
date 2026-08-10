package bleep
package commands
package server

import bleep.model.BspServerConfig
import ryddig.Logger

/** One knob, written to `~/.config/bleep/config.yaml`.
  *
  * The subcommands are bespoke — one per knob, with its own name, arity, metavar and help — because that is what makes `bleep server config parallelism 4`
  * discoverable and its validation specific. The *implementation* is this single class: the surface is per-knob, the code is not twelve copies.
  *
  * Every knob here is read once by the compile server when it starts, which is why each write says so. Silently writing a setting that will not take effect
  * until some unstated future moment is how people end up believing they have configured something they have not.
  */
case class ServerConfigSet(
    logger: Logger,
    userPaths: UserPaths,
    knob: String,
    update: BspServerConfig => BspServerConfig,
    /** Set when reached through the deprecated `bleep config compile-server ...` spelling. */
    deprecatedAlias: Option[String]
) extends BleepCommand {

  override def run(): Either[BleepException, Unit] = {
    deprecatedAlias.foreach(old => logger.warn(s"deprecated: `$old` — use `bleep server config $knob` instead"))

    BleepConfigOps
      .rewritePersisted(logger, userPaths)(config => config.copy(bspServerConfig = Some(update(config.bspServerConfigOrDefault))))
      .map { _ =>
        logger.info(s"$knob written to ${userPaths.configYaml}")
        // Read at startup, so a running server keeps its old value until it is replaced.
        logger.info("running servers keep their current setting — `bleep server restart` to apply now")
      }
  }
}
