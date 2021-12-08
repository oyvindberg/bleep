package bleep

import coursier._

object Deps {
  def Java(org: String, name: String, version: String): JavaOrScalaDependency.JavaDependency =
    JavaOrScalaDependency.JavaDependency(Organization(org), ModuleName(name), version)

  def Scala(org: String, name: String, version: String): JavaOrScalaDependency =
    JavaOrScalaDependency.ScalaDependency(
      Organization(org),
      ModuleName(name),
      version,
      fullCrossVersion = false,
      withPlatformSuffix = false
    )

  def ScalaFullVersion(org: String, name: String, version: String): JavaOrScalaDependency =
    JavaOrScalaDependency.ScalaDependency(
      Organization(org),
      ModuleName(name),
      version,
      fullCrossVersion = true,
      withPlatformSuffix = false
    )
}
