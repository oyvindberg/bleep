package bleep

package object tasks {
  type NativeImagePlugin = sbtnativeimage.NativeImagePlugin

  val PackagePlugin = bleep.packaging.PackagePlugin
  val DistPlugin = bleep.packaging.DistPlugin

}
