package sbtexport

import sbt._

object Compat {

  /** configurations exported besides Compile and Test. sbt 2 has no IntegrationTest */
  val extraConfigurations: Seq[Configuration] = Seq(IntegrationTest)
}
