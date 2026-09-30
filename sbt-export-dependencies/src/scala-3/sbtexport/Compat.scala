package sbtexport

import sbt.*

object Compat {

  /** configurations exported besides Compile and Test. sbt 2 has no IntegrationTest */
  val extraConfigurations: Seq[Configuration] = Nil
}
