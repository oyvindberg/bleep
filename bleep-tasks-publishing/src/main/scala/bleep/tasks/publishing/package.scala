package bleep.tasks

package object publishing {
  type CiReleasePlugin = com.geirsson.CiReleasePlugin
  type DynVerPlugin = sbtdynver.DynVerPlugin
  type GitVersioningPlugin = com.rallyhealth.sbt.versioning.GitVersioningPlugin
  type PgpPlugin = com.jsuereth.sbtpgp.PgpPlugin
  type Sonatype = xerial.sbt.Sonatype
  val Sonatype = xerial.sbt.Sonatype
}
