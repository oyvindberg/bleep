package bleep
package commands

import bleep.plugin.dynver.DynVerPlugin

import java.nio.file.Path

/** Where a publish gets its version: the caller says it, or bleep derives it from git.
  *
  * One field replacing two. `Publish.Options` used to carry `versionOverride: Option[String]` alongside `versionFallback: Option[() => String]`, which is a
  * two-field encoding of a one-of-two choice, with the "exactly one must be set" rule enforced at runtime and, on the Java side, thrown from a constructor. The
  * thunk existed so the command line could avoid shelling out to git when `--version` was given; that laziness belongs to the resolution below, not to the
  * shape of the input, and the Java bridge was collapsing it on the spot anyway.
  */
sealed trait PublishVersion

object PublishVersion {

  /** Publish exactly this. */
  case class Specified(value: String) extends PublishVersion

  /** Derive from git tags: `<tag>` on a clean tag, `<tag>+<distance>-<sha>[-SNAPSHOT]` otherwise. */
  case object Dynver extends PublishVersion

  /** The version to publish under, failing when `assertRelease` and git says this would be a snapshot.
    *
    * `assertRelease` deliberately does nothing for [[Specified]]. The caller spelled the version out, and bleep has no better source to contradict them with —
    * the flag is about what git state would produce. This preserves what the two commands did before, where the same rule was spelled `versionOverride.isEmpty`
    * in one and `versionOverride.isEmpty` again in the other, and answered "is this a snapshot" two different ways: `Publish` inspected the string for `+` and
    * `-SNAPSHOT` while `PublishSonatype` asked dynver. Now there is one answer, and it is dynver's.
    */
  def resolve(version: PublishVersion, buildDir: Path, assertRelease: Boolean): Either[BleepException, String] =
    version match {
      case Specified(value) => Right(value)
      case Dynver           =>
        val dynVer = dynverFor(buildDir)
        if (assertRelease && dynVer.isSnapshot)
          Left(
            new BleepException.Text(
              "--assert-release: version would be a snapshot. " +
                "Ensure you are on a clean git tag (no commits after tag, no dirty files). " +
                s"Current version: ${dynVer.version}"
            )
          )
        else Right(dynVer.version)
    }

  /** `dynverSonatypeSnapshots = true` to match the two other places that derive a version from git: `GenerateResources` (which bakes `BleepVersion.current`
    * into the client) and the Sonatype release path.
    *
    * Without it a snapshot publishes as `1.0.0-M10+46-abc1234` while the client built alongside it asks coursier for `1.0.0-M10+46-abc1234-SNAPSHOT` — so
    * `bleep publish local-ivy` produced jars no client would ever resolve, silently leaving the previously released server in play.
    */
  private[bleep] def dynverFor(buildDir: Path): DynVerPlugin =
    new DynVerPlugin(baseDirectory = buildDir.toFile, dynverSonatypeSnapshots = true)
}
