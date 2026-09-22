package bleep.model

import io.circe.{Decoder, Encoder}

import java.util.Properties

case class BleepVersion(value: String) extends AnyVal {
  def latestRelease: BleepVersion = BleepVersion(value.split("\\+").head)
  def isDevelopment: Boolean = latestRelease.value != value
}

object BleepVersion {
  val dev = BleepVersion("dev")

  /** The version of THIS bleep: what the client asks coursier for when it fetches its own server, and what `$version` in a build is compared against.
    *
    * Read from the `dynver` stamp bleep-model carries (`stamp: [dynver]` in bleep's own `bleep.yaml`), not baked into a generated constant. The constant was
    * issue #669 in bleep itself: a value derived from git is not an input the generator watches, so it went stale until someone ran `bleep sourcegen`; and
    * because generated sources are in the cache key, it made bleep-model and everything downstream of it — which is the whole build — a cache miss on every
    * commit. A stamp is recomputed on every build, sits in no cache key, and `bleep publish --version X` stamps X, so the published coordinate and the version
    * a binary reports cannot disagree.
    *
    * Lazy, so a missing stamp fails the one caller that needs the version rather than the object's initializer — `dev` and the codecs stay usable.
    */
  lazy val current: BleepVersion = BleepVersion(readStamp())

  private def readStamp(): String = {
    val resource = "/" + bleep.StampFile.resourcePath(CrossProjectName(ProjectName("bleep-model"), None))
    val in = classOf[BleepVersion].getResourceAsStream(resource)
    if (in == null)
      throw new IllegalStateException(
        s"bleep cannot tell its own version: $resource is not on the classpath. Either this bleep was built by one that does not write stamps (bleep-model " +
          "declares `stamp: [dynver]`, which needs a bleep new enough to honour it), or it is running from compiled classes without their generated resources."
      )
    try {
      val props = new Properties()
      props.load(in)
      Option(props.getProperty("dynver")).getOrElse(throw new IllegalStateException(s"$resource has no `dynver` entry: ${props.stringPropertyNames()}"))
    } finally in.close()
  }

  implicit val ordering: Ordering[BleepVersion] = Ordering.by(_.value)
  implicit val encodes: Encoder[BleepVersion] = Encoder[String].contramap(_.value)
  implicit val decodes: Decoder[BleepVersion] = Decoder[String].map(BleepVersion.apply)
}
