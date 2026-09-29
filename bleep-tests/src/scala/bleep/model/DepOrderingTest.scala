package bleep.model

import coursier.core.{Classifier, Configuration, Extension, ModuleName, Organization, Publication, Type}
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.SortedSet

/** `Dep.ordering` agrees with equality: dependencies compare as equal exactly when they are. A field which is not in the ordering makes two different
  * dependencies one to a sorted set
  */
class DepOrderingTest extends AnyFunSuite {
  private val java = Dep.Java("org.example", "lib", "1.0.0")
  private val scala = Dep.Scala("org.example", "lib", "1.0.0")
  private val publication = Publication("lib", Type("jar"), Extension("jar"), Classifier("tests"))
  private val exclusions = JsonMap(Map(Organization("org.a") -> JsonSet(ModuleName("x"))))

  private def assertDistinct(base: Dep, changed: List[(String, Dep)]): Unit =
    changed.foreach { case (field, dep) =>
      assert(dep != base, field)
      assert(Dep.ordering.compare(base, dep) != 0, s"$field is not in the ordering")
      assert(Dep.ordering.compare(base, dep) == -Dep.ordering.compare(dep, base), s"$field is not ordered both ways")
      assert(SortedSet[Dep](base, dep).size == 2, field)
    }

  test("every field of a java dependency is in the ordering") {
    assertDistinct(
      java,
      List(
        "organization" -> java.copy(organization = Organization("org.other")),
        "moduleName" -> java.copy(moduleName = ModuleName("other")),
        "version" -> java.copy(version = "2.0.0"),
        "attributes" -> java.copy(attributes = Map("k" -> "v")),
        "configuration" -> java.copy(configuration = Configuration.provided),
        "exclusions" -> java.copy(exclusions = exclusions),
        "publication" -> java.copy(publication = publication),
        "transitive" -> java.copy(transitive = false),
        "isSbtPlugin" -> java.copy(isSbtPlugin = true)
      )
    )
  }

  test("every field of a scala dependency is in the ordering") {
    assertDistinct(
      scala,
      List(
        "organization" -> scala.copy(organization = Organization("org.other")),
        "baseModuleName" -> scala.copy(baseModuleName = ModuleName("other")),
        "version" -> scala.copy(version = "2.0.0"),
        "fullCrossVersion" -> scala.copy(fullCrossVersion = true),
        "forceJvm" -> scala.copy(forceJvm = true),
        "for3Use213" -> scala.copy(for3Use213 = true),
        "for213Use3" -> scala.copy(for213Use3 = true),
        "attributes" -> scala.copy(attributes = Map("k" -> "v")),
        "configuration" -> scala.copy(configuration = Configuration.provided),
        "exclusions" -> scala.copy(exclusions = exclusions),
        "publication" -> scala.copy(publication = publication),
        "transitive" -> scala.copy(transitive = false),
        "isSbtPlugin" -> scala.copy(isSbtPlugin = true)
      )
    )
  }

  test("the same module in java and scala form is two dependencies") {
    assertDistinct(java, List("kind" -> scala))
  }

  test("case class fields: a new one fails here until it is in the ordering and in these tests") {
    assert(java.productArity == 9)
    assert(scala.productArity == 13)
  }
}
