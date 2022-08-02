package bleep

package object model {
  val $schema = "https://raw.githubusercontent.com/oyvindberg/bleep/master/schema.json"

  private[bleep] def assertUsed(anies: Any*): Unit = ((), anies)._1
}
