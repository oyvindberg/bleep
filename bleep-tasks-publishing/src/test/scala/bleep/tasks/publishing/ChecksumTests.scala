package bleep.tasks.publishing

import org.scalactic.TripleEqualsSupport
import org.scalatest.funsuite.AnyFunSuite

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

class ChecksumTests extends AnyFunSuite with TripleEqualsSupport {
  def resource(name: String) = Files.readAllBytes(Path.of(getClass.getResource(name).toURI))
  def str(name: String) = new String(resource(name), StandardCharsets.UTF_8)

  test("works") {
    val pom = resource("/shapeless_2.13-2.4.0-M1.pom")
    val sha1 = str("/shapeless_2.13-2.4.0-M1.pom.sha1")
    val md5 = str("/shapeless_2.13-2.4.0-M1.pom.md5")
    assert(Checksums.computeAsString(pom, Checksums.Algorithm.Sha1) === sha1)
    assert(Checksums.computeAsString(pom, Checksums.Algorithm.Md5) === md5)
  }
}
