package bleep.nosbt.io

import java.io.File

object Path {
  def fileProperty(name: String): File = new File(System.getProperty(name))

  def userHome: File = fileProperty("user.home")

}
