package bleep.sbtexport

import sjsonnew.JsonFormat

/** bleep's copy of sbt's librarymanagement, which [[ExportedProject]] is read with. Its twin in sbt-export-dependencies names sbt's own */
object Lm {
  type ModuleID = bleep.nosbt.librarymanagement.ModuleID
  type ExclusionRule = bleep.nosbt.librarymanagement.InclExclRule
  type CrossVersion = bleep.nosbt.librarymanagement.CrossVersion
  type ScalaVersion = bleep.nosbt.librarymanagement.ScalaVersion
  type Level = bleep.nosbt.util.Level.Value
  val Levels: bleep.nosbt.util.Level.type = bleep.nosbt.util.Level

  def scalaVersion(full: String, binary: String): ScalaVersion = bleep.nosbt.librarymanagement.ScalaVersion(full, binary)

  implicit val moduleIDFormat: JsonFormat[ModuleID] = bleep.nosbt.librarymanagement.LibraryManagementCodec.ModuleIDFormat
  implicit val exclusionRuleFormat: JsonFormat[ExclusionRule] = bleep.nosbt.librarymanagement.LibraryManagementCodec.InclExclRuleFormat
  implicit val crossVersionFormat: JsonFormat[CrossVersion] = bleep.nosbt.librarymanagement.LibraryManagementCodec.CrossVersionFormat
}
