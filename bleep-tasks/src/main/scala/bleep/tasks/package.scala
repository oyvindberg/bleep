package bleep

package object tasks {
  type GitVersioningPlugin = com.rallyhealth.sbt.versioning.GitVersioningPlugin
  type NativeImagePlugin = sbtnativeimage.NativeImagePlugin
  val PackagePlugin = net.hamnaberg.blooppackager.PackagePlugin
  val PackageCommand = net.hamnaberg.blooppackager.PackageCommand
}
