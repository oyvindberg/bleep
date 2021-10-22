package bleep

import coursier._
import coursier.parse.JavaOrScalaDependency

object Deps {
  def Java(org: String, name: String, version: String): JavaOrScalaDependency =
    JavaOrScalaDependency.JavaDependency(Dependency(Module(Organization(org), ModuleName(name)), version), Set.empty)

  def Scala(org: String, name: String, version: String): JavaOrScalaDependency =
    JavaOrScalaDependency.ScalaDependency(
      Dependency(Module(Organization(org), ModuleName(name)), version),
      fullCrossVersion = false,
      withPlatformSuffix = false,
      Set.empty
    )

  def ScalaFullVersion(org: String, name: String, version: String): JavaOrScalaDependency =
    JavaOrScalaDependency.ScalaDependency(
      Dependency(Module(Organization(org), ModuleName(name)), version),
      fullCrossVersion = true,
      withPlatformSuffix = false,
      Set.empty
    )

  def ScalaJs(org: String, name: String, version: String): JavaOrScalaDependency =
    JavaOrScalaDependency.ScalaDependency(
      Dependency(Module(Organization(org), ModuleName(name)), version),
      fullCrossVersion = false,
      withPlatformSuffix = true,
      Set.empty
    )
}
