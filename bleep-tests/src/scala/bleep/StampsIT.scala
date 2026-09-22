package bleep

import bleep.commands.{Publish, PublishVersion}
import bleep.packaging.ManifestCreator

import java.nio.file.{Files, Path}
import java.util.Properties
import java.util.zip.ZipFile
import scala.jdk.StreamConverters.*
import scala.util.Using

/** `stamp:` end to end: where the file goes, what is in it, who can see it, and what the published jar carries.
  *
  * The property everything here protects is that a stamp reaches the runtime classpath and the jar and never the compiler. The digest excludes stamps on that
  * premise — if javac could see one, excluding it from the cache key would let the cache serve classes built against a different value.
  */
class StampsIT extends IntegrationTestHarness {

  private val lib = model.CrossProjectName(model.ProjectName("lib"), None)
  private val app = model.CrossProjectName(model.ProjectName("app"), None)
  private val appTest = model.CrossProjectName(model.ProjectName("app-test"), None)

  private val StampedYaml =
    s"""projects:
       |  lib:
       |    platform:
       |      name: jvm
       |    stamp: [dynver, git-sha, project-digest, build-digest]
       |    publish:
       |      groupId: com.example
       |  app:
       |    platform:
       |      name: jvm
       |    dependsOn: lib
       |  app-test:
       |    platform:
       |      name: jvm
       |    dependsOn: app
       |    isTestProject: true
       |    dependencies: org.junit.jupiter:junit-jupiter:${model.Versions.JunitJupiter}
       |""".stripMargin

  /** Reads a DEPENDENCY's stamp at runtime, in the test fork. `lib` is two hops away, so this only passes if the stamp dir survived `ResolveProjects`, the wire
    * to the server, and the fork's classpath, tagged `Runtime` all the way.
    */
  private val ReadsLibStamp =
    """package com.example;
      |
      |import static org.junit.jupiter.api.Assertions.assertNotNull;
      |import static org.junit.jupiter.api.Assertions.assertTrue;
      |
      |import java.io.InputStream;
      |import java.util.Properties;
      |import org.junit.jupiter.api.Test;
      |
      |public class ReadsLibStampTest {
      |  @Test
      |  void seesItsDependencysStamp() throws Exception {
      |    try (InputStream in = ReadsLibStampTest.class.getResourceAsStream("/bleep-stamp/lib.properties")) {
      |      assertNotNull(in, "lib's stamp is not on the test fork's classpath");
      |      Properties p = new Properties();
      |      p.load(in);
      |      assertTrue(p.getProperty("git-sha").matches("[0-9a-f]{40}"), "git-sha was " + p.getProperty("git-sha"));
      |    }
      |  }
      |}
      |""".stripMargin

  private def git(root: Path, args: String*): String = {
    val out = new StringBuilder
    val exitCode = scala.sys.process.Process("git" :: args.toList, root.toFile).!(scala.sys.process.ProcessLogger(line => out.append(line), _ => ()))
    if (exitCode != 0) fail(s"git ${args.mkString(" ")} failed with exit code $exitCode")
    out.toString.trim
  }

  /** A workspace that is a git repository with one commit, so `dynver` and `git-sha` have something to describe. `.bleep/` is ignored so building does not
    * dirty the tree between two compiles that must produce the same stamp.
    */
  private def setUp(ws: Workspace): Unit = {
    ws.yaml(StampedYaml, snippet = "stamps/bleep.yaml")
    ws.file("lib/src/java/com/example/Lib.java", "package com.example; public class Lib {}")
    ws.file("app/src/java/com/example/App.java", "package com.example; public class App { Lib lib; }")
    ws.file("app-test/src/java/com/example/ReadsLibStampTest.java", ReadsLibStamp, snippet = "stamps/ReadsLibStampTest.java")
    ws.file(".gitignore", ".bleep/\n")
    git(ws.root, "init")
    git(ws.root, "add", "-A")
    git(ws.root, "-c", "user.email=ci@bleep.build", "-c", "user.name=bleep", "-c", "commit.gpgsign=false", "commit", "-m", "base")
  }

  private def read(file: Path): Properties = {
    val p = new Properties()
    Using.resource(Files.newInputStream(file))(p.load)
    p
  }

  integrationTest("a stamp reaches the runtime classpath and never the compiler, for its own project and for dependents") { ws =>
    setUp(ws)
    val (started, _, _) = ws.start()
    val libStamps = started.projectPaths(lib).resourcesDirs.stamps.getOrElse(fail("lib declared stamps but has no stamps directory"))

    // Its own project: packaged and on the runtime classpath, not an input.
    assert(started.resolvedProject(lib).resources(Usage.Runtime).contains(libStamps))
    assert(!started.resolvedProject(lib).resources(Usage.Compile).contains(libStamps))
    assert(!ProjectInputs.all(started.build.explodedProjects(lib), started.projectPaths(lib)).contains(libStamps), "watch and `build invalidated` see it")

    // A dependent: on the runtime classpath, which is how `app` would read it, and absent from the one javac is given.
    assert(started.resolvedProject(app).classpath(Usage.Runtime).contains(libStamps))
    assert(!started.resolvedProject(app).classpath(Usage.Compile).contains(libStamps))

    // The load-bearing sweep: no stamps directory, of any project, on any project's compile classpath. This is what licenses excluding stamps from the digest.
    val allStampDirs = started.build.explodedProjects.keys.flatMap(cn => started.projectPaths(cn).resourcesDirs.stamps).toSet
    started.build.explodedProjects.keys.foreach { cn =>
      val leaked = started.resolvedProject(cn).classpath(Usage.Compile).filter(allStampDirs)
      assert(leaked.isEmpty, s"${cn.value}'s compile classpath carries stamps: $leaked")
    }
    succeed
  }

  integrationTest("the file carries exactly the declared values, readable by java.util.Properties, byte-stable between builds") { ws =>
    setUp(ws)
    val (started, commands, _) = ws.start()
    commands.compile(List(lib, app)).discard()

    val file = Stamps.fileFor(started, lib)
    assert(file.toString.endsWith("bleep-stamps/bleep-stamp/lib.properties"), file)
    val stamp = read(file)

    assert(stamp.stringPropertyNames().toArray.toSet == Set("dynver", "git-sha", "project-digest", "build-digest"), stamp)
    assert(stamp.getProperty("git-sha") == git(ws.root, "rev-parse", "HEAD"))
    assert(stamp.getProperty("dynver") == PublishVersion.resolve(PublishVersion.Dynver, ws.root, assertRelease = false).orThrow)
    assert(stamp.getProperty("project-digest") == ProjectDigest.computeAll(started.build, started.buildPaths)(lib))
    assert(stamp.getProperty("build-digest").matches("[0-9a-f]{64}"), stamp.getProperty("build-digest"))

    // `app` declared no stamps: no directory in its path model, so nothing to write and nothing on its classpath.
    assert(started.projectPaths(app).resourcesDirs.stamps.isEmpty)

    // Same git state, same bytes — and not rewritten at all, so nothing watching mtimes is disturbed.
    val bytesBefore = Files.readAllBytes(file)
    val mtimeBefore = Files.getLastModifiedTime(file)
    commands.compile(List(lib, app)).discard()
    assert(Files.readAllBytes(file).sameElements(bytesBefore))
    assert(Files.getLastModifiedTime(file) == mtimeBefore)
  }

  integrationTest("a test reads its dependency's stamp at runtime, two hops away, through the fork's classpath") { ws =>
    setUp(ws)
    val (_, commands, storingLogger) = ws.start()
    commands.test(projects = List(appTest), watch = false, only = None, exclude = None, includeTags = None, excludeTags = None).discard()
    assertSuitePassed(storingLogger, "com.example.ReadsLibStampTest", tests = 1)
  }

  integrationTest("publish --version X stamps X into the jar, so the coordinate and the value inside cannot disagree") { ws =>
    // Issue #669: a version baked in at compile time and a version chosen at publish time were two derivations that could drift apart.
    setUp(ws)
    val (_, commands, _) = ws.start()
    val repo = Files.createTempDirectory("bleep-stamps-ivy-")
    commands.publish(
      Publish.Options(
        version = PublishVersion.Specified("9.9.9-stamped"),
        assertRelease = false,
        dryRun = false,
        target = Publish.Target.LocalIvy(Some(repo)),
        projectNames = Array(lib),
        manifestCreator = ManifestCreator.default
      )
    )

    val jar = Using
      .resource(Files.walk(repo))(_.toScala(List))
      .find(p => p.getFileName.toString.endsWith(".jar") && !p.getFileName.toString.matches(".*-(sources|javadoc)\\.jar"))
      .getOrElse(fail(s"no jar published under $repo"))
    val stamp = Using.resource(new ZipFile(jar.toFile)) { zip =>
      val entry = Option(zip.getEntry("bleep-stamp/lib.properties")).getOrElse(fail(s"$jar has no bleep-stamp/lib.properties"))
      val p = new Properties()
      Using.resource(zip.getInputStream(entry))(p.load)
      p
    }
    assert(stamp.getProperty("dynver") == "9.9.9-stamped", stamp)
  }

  integrationTest("a version a .properties reader would not return verbatim is refused, not mangled") { ws =>
    setUp(ws)
    val (started, _, _) = ws.start()
    val thrown = intercept[BleepException](Stamps.materialize(started, publishingAs = Some("1.0\nsneaky=true")))
    assert(thrown.getMessage.contains("would not get back verbatim"), thrown.getMessage)
  }

  integrationTest("a forked script JVM can read BleepVersion.current, which it now gets from bleep-model's stamp") { ws =>
    // A sourcegen runs in its own JVM, on a classpath the parent builds from the script project's resolved dependencies — for a `build.bleep:*` dependency,
    // bleep's own classes plus their resource directories (`ResolveProjects.ReplaceBleepDependencies`). bleep-model's stamps root has to be among those, or
    // its classes arrive without the version they now read, and anything the script does that needs bleep's own version fails. This generator reads it and
    // writes it out, so the test sees exactly what the forked JVM saw.
    ws.yaml(
      s"""projects:
         |  myapp:
         |    platform:
         |      name: jvm
         |    scala:
         |      version: ${model.VersionScala.Scala3.scalaVersion}
         |    sourcegen:
         |      project: scripts
         |      main: scripts.GenVersion
         |  scripts:
         |    dependencies:
         |      - build.bleep::bleep-core:$${BLEEP_VERSION}
         |    platform:
         |      name: jvm
         |    scala:
         |      version: ${model.VersionScala.Scala3.scalaVersion}
         |""".stripMargin
    )
    ws.file(
      "scripts/src/scala/scripts/GenVersion.scala",
      """package scripts
        |
        |import bleep.*
        |
        |import java.nio.file.Files
        |
        |object GenVersion extends BleepCodegenScript("GenVersion") {
        |  override def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit =
        |    targets.foreach { target =>
        |      val file = target.sources.resolve("generated/SeenVersion.scala")
        |      Files.createDirectories(file.getParent)
        |      Files.writeString(file, s"package generated\nobject SeenVersion { val value = \"${model.BleepVersion.current.value}\" }\n")
        |    }
        |}
        |""".stripMargin
    )
    ws.file("myapp/src/scala/App.scala", "object App { def seen: String = generated.SeenVersion.value }")

    val (started, commands, _) = ws.start()
    commands.compile(List(model.CrossProjectName(model.ProjectName("myapp"), None))).discard()

    val generatedFile = Using
      .resource(Files.walk(started.buildPaths.dotBleepDir))(_.toScala(List))
      .find(_.getFileName.toString == "SeenVersion.scala")
      .getOrElse(fail("the generator did not run"))
    assert(Files.readString(generatedFile).contains(s"\"${model.BleepVersion.current.value}\""), Files.readString(generatedFile))
  }
}
