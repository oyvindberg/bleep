package bleep

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

class PostCompileIT extends IntegrationTestHarness with org.scalatest.matchers.should.Matchers {
  private def cn(name: String) = model.CrossProjectName(model.ProjectName(name), None)

  private def yaml(ws: Workspace): Unit =
    ws.yaml(
      s"""projects:
         |  app:
         |    dependsOn: lib
         |    java:
         |      version: "${model.Jvm.graalvm.majorVersion}"
         |    platform:
         |      name: jvm
         |      mainClass: app.Main
         |  lib:
         |    java:
         |      version: "${model.Jvm.graalvm.majorVersion}"
         |    platform:
         |      name: jvm
         |    postCompile:
         |      project: post
         |      main: post.Listing
         |      inputs: [input]
         |  input:
         |    java:
         |      version: "${model.Jvm.graalvm.majorVersion}"
         |    platform:
         |      name: jvm
         |  post:
         |    java:
         |      version: "${model.Jvm.graalvm.majorVersion}"
         |    platform:
         |      name: jvm
         |""".stripMargin
    )

  /** Copies the compiler's output, and adds `listing.txt`: every file it copied, then every file of each input. Fails when asked to, by a class named Boom. */
  private val listingScript =
    """package post;
      |
      |import java.io.IOException;
      |import java.nio.file.*;
      |import java.util.*;
      |import java.util.stream.Stream;
      |
      |public final class Listing {
      |  public static void main(String[] args) throws IOException {
      |    Path from = null, to = null;
      |    Map<String, Path> inputs = new TreeMap<>();
      |    for (int i = 0; i < args.length; i++) {
      |      switch (args[i]) {
      |        case "--from" -> from = Path.of(args[++i]);
      |        case "--to" -> to = Path.of(args[++i]);
      |        case "--classpath" -> i++;
      |        case "--input" -> { String[] kv = args[++i].split("=", 2); inputs.put(kv[0], Path.of(kv[1])); }
      |        default -> throw new IllegalArgumentException(args[i]);
      |      }
      |    }
      |    List<String> lines = new ArrayList<>();
      |    for (Path p : files(from)) {
      |      String rel = from.relativize(p).toString();
      |      if (rel.endsWith("Boom.class")) throw new IllegalStateException("refusing to post-compile a Boom");
      |      Path out = to.resolve(rel);
      |      Files.createDirectories(out.getParent());
      |      Files.copy(p, out);
      |      lines.add("lib " + rel);
      |    }
      |    for (Map.Entry<String, Path> e : inputs.entrySet())
      |      for (Path p : files(e.getValue())) lines.add(e.getKey() + " " + e.getValue().relativize(p));
      |    Files.writeString(to.resolve("listing.txt"), String.join("\n", lines));
      |  }
      |
      |  private static List<Path> files(Path dir) throws IOException {
      |    try (Stream<Path> s = Files.walk(dir)) { return s.filter(Files::isRegularFile).sorted().toList(); }
      |  }
      |}
      |""".stripMargin

  private def listing(started: Started): List[String] =
    Files.readAllLines(started.projectPaths(cn("lib")).classes.resolve("listing.txt")).asScala.toList

  private def filesUnder(dir: Path): Set[String] = {
    val s = Files.walk(dir)
    try s.iterator().asScala.filter(Files.isRegularFile(_)).map(p => dir.relativize(p).toString).toSet
    finally s.close()
  }

  integrationTest("post-compile: consumers see the script's output, the compiler's stays private, and it reruns exactly when its inputs change") { ws =>
    yaml(ws)
    ws.file("post/src/java/post/Listing.java", listingScript)
    ws.file("input/src/java/in/Data.java", "package in;\npublic final class Data {}\n")
    ws.file("lib/src/java/lib/Lib.java", "package lib;\npublic final class Lib { public static String hello() { return \"hello\"; } }\n")
    ws.file("lib/src/java/lib/Extra.java", "package lib;\npublic final class Extra {}\n")
    ws.file(
      "app/src/java/app/Main.java",
      """package app;
        |
        |public final class Main {
        |  public static void main(String[] args) throws Exception {
        |    try (var in = Main.class.getClassLoader().getResourceAsStream("listing.txt")) {
        |      System.out.println(lib.Lib.hello() + " / " + new String(in.readAllBytes()).replace('\n', ','));
        |    }
        |  }
        |}
        |""".stripMargin
    )

    val (started, commands, storingLogger) = ws.start()
    val libPaths = started.projectPaths(cn("lib"))
    commands.compile(List(cn("app")))

    // The script's output is what `classes` holds; the compiler wrote elsewhere, and only there
    assert(libPaths.compilerOutput != libPaths.classes)
    filesUnder(libPaths.classes) shouldBe Set("lib/Lib.class", "lib/Extra.class", "listing.txt")
    filesUnder(libPaths.compilerOutput) shouldBe Set("lib/Lib.class", "lib/Extra.class")
    listing(started) shouldBe List("lib lib/Extra.class", "lib lib/Lib.class", "input in/Data.class")

    // ... and it is what a consumer runs against
    commands.run(cn("app"))
    assert(storingLogger.underlying.exists(_.message.plainText.startsWith("hello / lib lib/Extra.class,lib lib/Lib.class,input in/Data.class")))

    // Nothing changed: the script does not run again
    val stampBefore = Files.readString(libPaths.postCompileStamp)
    val listingMtime = Files.getLastModifiedTime(libPaths.classes.resolve("listing.txt"))
    commands.compile(List(cn("app")))
    Files.readString(libPaths.postCompileStamp) shouldBe stampBefore
    Files.getLastModifiedTime(libPaths.classes.resolve("listing.txt")) shouldBe listingMtime

    // An input changed: lib itself does not recompile, but its post-compile reruns
    ws.file("input/src/java/in/More.java", "package in;\npublic final class More {}\n")
    commands.compile(List(cn("app")))
    listing(started) should contain("input in/More.class")

    // A class deleted from the source is gone from `classes` too — the script's output is complete, bleep deletes the rest
    Files.delete(ws.root.resolve("lib/src/java/lib/Extra.java"))
    commands.compile(List(cn("app")))
    filesUnder(libPaths.classes) shouldBe Set("lib/Lib.class", "listing.txt")

    // A failing script fails the compile and leaves `classes` unvouched for
    ws.file("lib/src/java/lib/Boom.java", "package lib;\npublic final class Boom {}\n")
    intercept[BleepException](commands.compile(List(cn("app"))))
    assert(!Files.exists(libPaths.postCompileStamp))

    // Fixed: back to green
    Files.delete(ws.root.resolve("lib/src/java/lib/Boom.java"))
    commands.compile(List(cn("app")))
    filesUnder(libPaths.classes) shouldBe Set("lib/Lib.class", "listing.txt")
    succeed
  }

  /** Copies the compiler's output, then every class of every input on top: a transform that changes the API consumers see, like patching classes into a
    * library.
    */
  private def copyInputsScript(extraLine: String): String =
    s"""package post;
       |
       |import java.io.IOException;
       |import java.nio.file.*;
       |import java.util.stream.Stream;
       |
       |public final class CopyInputs {
       |  public static void main(String[] args) throws IOException {
       |    Path to = null;
       |    java.util.List<Path> sources = new java.util.ArrayList<>();
       |    for (int i = 0; i < args.length; i++) {
       |      switch (args[i]) {
       |        case "--from" -> sources.add(0, Path.of(args[++i]));
       |        case "--to" -> to = Path.of(args[++i]);
       |        case "--classpath" -> i++;
       |        case "--input" -> sources.add(Path.of(args[++i].split("=", 2)[1]));
       |        default -> throw new IllegalArgumentException(args[i]);
       |      }
       |    }
       |    $extraLine
       |    for (Path from : sources)
       |      try (Stream<Path> s = Files.walk(from)) {
       |        for (Path p : (Iterable<Path>) s::iterator) {
       |          if (!Files.isRegularFile(p)) continue;
       |          Path out = to.resolve(from.relativize(p).toString());
       |          Files.createDirectories(out.getParent());
       |          Files.copy(p, out, StandardCopyOption.REPLACE_EXISTING);
       |        }
       |      }
       |  }
       |}
       |""".stripMargin

  integrationTest("post-compile: a consumer recompiles exactly the sources that use a class the transform changed, in Java and in Scala") { ws =>
    val jv = model.Jvm.graalvm.majorVersion
    ws.yaml(
      s"""projects:
         |  app:
         |    dependsOn: lib
         |    java: { version: "$jv" }
         |    platform: { name: jvm }
         |  sapp:
         |    dependsOn: lib
         |    java: { version: "$jv" }
         |    platform: { name: jvm }
         |    scala: { version: ${model.VersionScala.Scala3.scalaVersion} }
         |  lib:
         |    java: { version: "$jv" }
         |    platform: { name: jvm }
         |    postCompile:
         |      project: post
         |      main: post.CopyInputs
         |      inputs: [extra]
         |  extra:
         |    java: { version: "$jv" }
         |    platform: { name: jvm }
         |  post:
         |    java: { version: "$jv" }
         |    platform: { name: jvm }
         |""".stripMargin
    )
    ws.file("post/src/java/post/CopyInputs.java", copyInputsScript(""))
    // `extra` provides a class the transform adds, and its own `lib.Patched`, which replaces lib's — like patching classes into a library
    def added(members: String) = ws.file("extra/src/java/extra/Added.java", s"package extra;\npublic final class Added { $members }\n")
    def patched(members: String) = ws.file("extra/src/java/lib/Patched.java", s"package lib;\npublic final class Patched { $members }\n")
    added("public static int value() { return 1; }")
    patched("public static int a() { return 1; }")
    ws.file("lib/src/java/lib/Lib.java", "package lib;\npublic final class Lib { public static String hello() { return \"hello\"; } }\n")
    ws.file("lib/src/java/lib/Patched.java", "package lib;\npublic final class Patched { public static int a() { return 0; } }\n")
    // One consumer source per kind of use: an untouched class, a class the transform adds, a class it replaces
    ws.file("app/src/java/app/A.java", "package app;\npublic final class A { String x() { return lib.Lib.hello(); } }\n")
    ws.file("app/src/java/app/B.java", "package app;\npublic final class B { int x() { return extra.Added.value(); } }\n")
    ws.file("app/src/java/app/C.java", "package app;\npublic final class C { int x() { return lib.Patched.a(); } }\n")
    ws.file("sapp/src/scala/sapp/SA.scala", "package sapp\nclass SA { def x: String = lib.Lib.hello() }\n")
    ws.file("sapp/src/scala/sapp/SB.scala", "package sapp\nclass SB { def x: Int = extra.Added.value() }\n")
    ws.file("sapp/src/scala/sapp/SC.scala", "package sapp\nclass SC { def x: Int = lib.Patched.a() }\n")

    val (started, commands, _) = ws.start()
    val consumers = List(cn("app"), cn("sapp"))
    val watched: List[(String, Path)] =
      List("A", "B", "C").map(n => n -> started.projectPaths(cn("app")).classes.resolve(s"app/$n.class")) ++
        List("SA", "SB", "SC").map(n => n -> started.projectPaths(cn("sapp")).classes.resolve(s"sapp/$n.class"))
    def stamps(): Map[String, java.nio.file.attribute.FileTime] = watched.map { case (n, p) => n -> Files.getLastModifiedTime(p) }.toMap

    /** Which of the watched consumer classes were rewritten since `before` */
    def recompiledSince(before: Map[String, java.nio.file.attribute.FileTime]): Set[String] = stamps().collect { case (n, t) if t != before(n) => n }.toSet

    commands.compile(consumers)
    val libClasses = started.projectPaths(cn("lib")).classes

    // A body-only change to an added class: the transform reran, its API effect did not change — nobody recompiles
    var before = stamps()
    val addedBytes = Files.readAllBytes(libClasses.resolve("extra/Added.class"))
    added("public static int value() { return 2; }")
    commands.compile(consumers)
    assert(!java.util.Arrays.equals(Files.readAllBytes(libClasses.resolve("extra/Added.class")), addedBytes), "the transform reran")
    recompiledSince(before) shouldBe Set.empty

    // The script itself changed, to no effect on the API — nobody recompiles
    before = stamps()
    ws.file("post/src/java/post/CopyInputs.java", copyInputsScript("System.out.println(\"copying\");"))
    commands.compile(consumers)
    recompiledSince(before) shouldBe Set.empty

    // An added class gained API: only the sources using it recompile
    before = stamps()
    added("public static int value() { return 2; } public static int more() { return 3; }")
    commands.compile(consumers)
    recompiledSince(before) shouldBe Set("B", "SB")

    // A replaced class gained API that lib's own source does not have: only the sources using it recompile
    before = stamps()
    patched("public static int a() { return 1; } public static int b() { return 2; }")
    commands.compile(consumers)
    recompiledSince(before) shouldBe Set("C", "SC")
    succeed
  }
}
