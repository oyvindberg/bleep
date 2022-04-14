package bleep

package object tasks {
  type GitVersioningPlugin = com.rallyhealth.sbt.versioning.GitVersioningPlugin
  type NativeImagePlugin = sbtnativeimage.NativeImagePlugin
  val PackagePlugin = bleep.packaging.PackagePlugin
  val PackageCommand = bleep.packaging.PackageCommand
}
