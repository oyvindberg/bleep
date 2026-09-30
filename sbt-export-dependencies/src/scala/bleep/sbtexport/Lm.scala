package bleep.sbtexport

import sjsonnew.JsonFormat

/** sbt's librarymanagement, which [[ExportedProject]] is written with inside sbt. Its twin in bleep-cli names bleep's copy of it */
object Lm {
  type ModuleID = sbt.librarymanagement.ModuleID
  type ExclusionRule = sbt.librarymanagement.InclExclRule
  type CrossVersion = sbt.librarymanagement.CrossVersion
  type ScalaVersion = sbt.librarymanagement.ScalaVersion
  type Level = sbt.util.Level.Value
  val Levels: sbt.util.Level.type = sbt.util.Level

  def scalaVersion(full: String, binary: String): ScalaVersion = sbt.librarymanagement.ScalaVersion(full, binary)

  implicit val moduleIDFormat: JsonFormat[ModuleID] = sbt.librarymanagement.LibraryManagementCodec.ModuleIDFormat
  implicit val exclusionRuleFormat: JsonFormat[ExclusionRule] = sbt.librarymanagement.LibraryManagementCodec.InclExclRuleFormat
  implicit val crossVersionFormat: JsonFormat[CrossVersion] = sbt.librarymanagement.LibraryManagementCodec.CrossVersionFormat
}
