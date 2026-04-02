package bleep

import bleep.internal.FileUtils
import org.scalactic.TripleEqualsSupport
import org.scalatest.funsuite.AnyFunSuite

/** Verifies that the hardcoded artifact mappings in BleepDevDeps match bleep's own build. */
class BleepDevDepsTest extends AnyFunSuite with TripleEqualsSupport {

  lazy val build: model.Build.FileBacked = {
    val existing = BuildLoader.find(FileUtils.cwd).existing.orThrow
    val buildFile = existing.buildFile.forceGet("BleepDevDepsTest").orThrow
    model.Build.FileBacked(buildFile)
  }

  test("every artifact maps to an existing project") {
    val projectNames = build.explodedProjects.keySet
    BleepDevDeps.artifacts.foreach { case (artifactName, crossName) =>
      assert(projectNames.contains(crossName), s"artifact '$artifactName' maps to ${crossName.value} which doesn't exist in build")
    }
  }

  test("all cross IDs are None") {
    BleepDevDeps.artifacts.foreach { case (artifactName, crossName) =>
      assert(crossName.crossId.isEmpty, s"artifact '$artifactName' has non-empty crossId: ${crossName.crossId}")
    }
  }

  test("transitive bleep deps match build model") {
    val artifactNameSet = BleepDevDeps.artifacts.keySet

    BleepDevDeps.transitiveBleepDeps.foreach { case (artifactName, expectedTransitiveDeps) =>
      val crossName = BleepDevDeps.artifacts(artifactName)
      val actualTransitiveDeps = build.transitiveDependenciesFor(crossName).keySet
        .map(_.name.value)
        .filter(artifactNameSet.contains)

      assert(
        actualTransitiveDeps === expectedTransitiveDeps,
        s"transitive bleep deps mismatch for '$artifactName': actual=$actualTransitiveDeps, expected=$expectedTransitiveDeps"
      )
    }
  }

  test("every artifact has a transitiveBleepDeps entry") {
    BleepDevDeps.artifacts.keys.foreach { artifactName =>
      assert(BleepDevDeps.transitiveBleepDeps.contains(artifactName), s"artifact '$artifactName' missing from transitiveBleepDeps")
    }
  }

  test("transitiveBleepDeps only references known artifacts") {
    val knownArtifacts = BleepDevDeps.artifacts.keySet
    BleepDevDeps.transitiveBleepDeps.foreach { case (artifactName, deps) =>
      deps.foreach { dep =>
        assert(knownArtifacts.contains(dep), s"transitiveBleepDeps for '$artifactName' references unknown artifact '$dep'")
      }
    }
  }
}
