package bleep

import bleep.commands.PublishVersion
import bleep.internal.bleepLoggers
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}

/** Where a publish gets its version.
  *
  * The two cases are asserted separately because they differ in more than their source: `--assert-release` applies to the version bleep derives and not to one
  * the caller spelled out, which used to be spelled `versionOverride.isEmpty` in two commands that then disagreed about how to decide whether a version was a
  * snapshot — one inspected the string, the other asked dynver.
  */
class PublishVersionTest extends AnyFunSuite {

  private val logger = bleepLoggers.silent

  private def git(cwd: Path, args: String*): Unit =
    cli(action = "git", cwd = cwd, cmd = "git" :: args.toList, logger = logger, out = cli.Out.ViaLogger(logger)).discard()

  /** A git repo with one commit, tagged `v1.2.3`, and dynver's own environment neutralised so a tag really is what decides. */
  private def gitRepo(f: Path => Unit): Unit = {
    val dir = Files.createTempDirectory("bleep-publish-version-")
    try {
      git(dir, "init", "--initial-branch", "main")
      git(dir, "config", "user.email", "test@example.com")
      git(dir, "config", "user.name", "test")
      Files.writeString(dir.resolve("a.txt"), "one")
      git(dir, "add", ".")
      git(dir, "commit", "-m", "one")
      git(dir, "tag", "v1.2.3")
      f(dir)
    } finally bleep.internal.FileUtils.deleteDirectory(dir)
  }

  test("a specified version is published as given") {
    gitRepo { dir =>
      assert(PublishVersion.resolve(PublishVersion.Specified("9.9.9"), dir, assertRelease = false) == Right("9.9.9"))
    }
  }

  test("--assert-release does not second-guess a version the caller spelled out") {
    // Deliberate, and preserved from before this was an ADT: the flag is about what git state would produce, and bleep has no better source to contradict an
    // explicit version with. A caller who types a snapshot version has said what they want.
    gitRepo { dir =>
      assert(PublishVersion.resolve(PublishVersion.Specified("1.0.0+3-abcdef-SNAPSHOT"), dir, assertRelease = true) == Right("1.0.0+3-abcdef-SNAPSHOT"))
    }
  }

  test("dynver reads the tag, and lives inside bleep") {
    // The point of the ADT: no supplier is passed in, and no caller has to know how to ask git. `Dynver` names the intent and bleep does the work.
    gitRepo { dir =>
      assert(PublishVersion.resolve(PublishVersion.Dynver, dir, assertRelease = false) == Right("1.2.3"))
    }
  }

  test("--assert-release fails on a dirty tree") {
    gitRepo { dir =>
      Files.writeString(dir.resolve("a.txt"), "two")
      PublishVersion.resolve(PublishVersion.Dynver, dir, assertRelease = true) match {
        case Right(version) => fail(s"a dirty tree should not pass --assert-release, got version $version")
        case Left(err)      =>
          assert(err.message.contains("--assert-release"), s"unexpected message: ${err.message}")
          assert(err.message.contains("snapshot"), s"the message should say what is wrong: ${err.message}")
      }
    }
  }

  test("a clean tag passes --assert-release") {
    gitRepo { dir =>
      assert(PublishVersion.resolve(PublishVersion.Dynver, dir, assertRelease = true) == Right("1.2.3"))
    }
  }
}
