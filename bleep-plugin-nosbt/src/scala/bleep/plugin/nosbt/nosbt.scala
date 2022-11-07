package bleep.plugin

import java.io.File

package object nosbt {
  implicit final class FileOps(val asFile: File) extends AnyVal {
    def /(component: String): File = if (component == ".") asFile else new File(asFile, component)
  }
}
