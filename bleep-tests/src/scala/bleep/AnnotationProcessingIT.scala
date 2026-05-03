package bleep

import java.nio.file.Files

class AnnotationProcessingIT extends IntegrationTestHarness {
  integrationTest("default off — Lombok in deps does not run, javac sees -proc:none") { ws =>
    ws.yaml(
      """projects:
        |  a:
        |    dependencies: org.projectlombok:lombok:1.18.46
        |    source-layout: java
        |    java:
        |      options: -Xlint:all
        |""".stripMargin
    )
    ws.file(
      "a/src/java/test/Person.java",
      """package test;
        |public class Person {
        |    private String name;
        |    private int age;
        |}""".stripMargin
    )
    val (started, _, storingLogger) = ws.start()
    val projectName = model.CrossProjectName(model.ProjectName("a"), None)
    val resolved = started.resolvedProjects(projectName).forceGet("test")

    val javaOptions = resolved.language.javaOptions
    assert(javaOptions.contains("-proc:none"), s"expected -proc:none in $javaOptions")
    assert(!javaOptions.exists(_.contains("-processorpath")), s"unexpected -processorpath in $javaOptions")
    assert(!javaOptions.exists(_.startsWith("-s")), s"unexpected -s in $javaOptions")
    assert(javaOptions.contains("-Xlint:all"))
    assert(!resolved.sources.exists(_.toString.contains("generated-sources")))
    assert(!storingLogger.underlying.exists(_.message.plainText.contains("auto-discovered annotation processor")))
  }

  integrationTest("scanForAnnotationProcessors: true reserves gen-sources dir, drops -proc:none") { ws =>
    ws.yaml(
      """projects:
        |  a:
        |    dependencies: org.projectlombok:lombok:1.18.46
        |    source-layout: java
        |    java:
        |      scanForAnnotationProcessors: true
        |""".stripMargin
    )
    ws.file("a/src/java/test/Person.java", "package test; public class Person {}")
    val (started, _, _) = ws.start()
    val projectName = model.CrossProjectName(model.ProjectName("a"), None)
    val resolved = started.resolvedProjects(projectName).forceGet("test")

    // Bootstrap state: AP flags are NOT in javaOptions — they get computed by the AP DAG
    // task at compile time. Only the gen-sources reservation is visible at bootstrap.
    val javaOptions = resolved.language.javaOptions
    assert(!javaOptions.contains("-proc:none"), s"-proc:none must NOT be set when AP is configured, got $javaOptions")
    assert(resolved.sources.exists(_.toString.contains("generated-sources")), s"expected gen-sources in ${resolved.sources}")
  }

  integrationTest("end-to-end with Mapstruct (split-jar processor): generated UserMapperImpl appears") { ws =>
    ws.yaml(
      """projects:
        |  a:
        |    dependencies: org.mapstruct:mapstruct:1.5.5.Final
        |    source-layout: java
        |    platform:
        |      name: jvm
        |    java:
        |      annotationProcessors:
        |        - org.mapstruct:mapstruct-processor:1.5.5.Final
        |      annotationProcessorOptions:
        |        mapstruct.suppressGeneratorTimestamp: "true"
        |""".stripMargin
    )
    ws.file(
      "a/src/java/test/User.java",
      """package test;
        |public class User {
        |    public String name;
        |    public User(String name) { this.name = name; }
        |}""".stripMargin
    )
    ws.file(
      "a/src/java/test/UserDto.java",
      """package test;
        |public class UserDto {
        |    public String name;
        |}""".stripMargin
    )
    ws.file(
      "a/src/java/test/UserMapper.java",
      """package test;
        |import org.mapstruct.Mapper;
        |@Mapper
        |public interface UserMapper {
        |    UserDto userToUserDto(User user);
        |}""".stripMargin
    )

    val (started, commands, _) = ws.start()
    val projectName = model.CrossProjectName(model.ProjectName("a"), None)
    val resolved = started.resolvedProjects(projectName).forceGet("test")

    // mapstruct-processor must NOT be on the runtime classpath — annotationProcessors entries
    // resolve in isolation and are passed to javac via -processorpath only.
    val classpathPaths = resolved.classpath.map(_.toString)
    assert(
      !classpathPaths.exists(_.contains("mapstruct-processor")),
      s"mapstruct-processor must NOT be on runtime classpath, got $classpathPaths"
    )

    commands.compile(List(projectName))

    val genDir = resolved.sources
      .find(_.toString.contains("generated-sources"))
      .getOrElse(sys.error(s"expected a generated-sources dir in ${resolved.sources}"))
    val generated = genDir.resolve("test").resolve("UserMapperImpl.java")
    assert(Files.isRegularFile(generated), s"expected UserMapperImpl.java at $generated")
  }

  integrationTest("explicit annotationProcessors list runs, scanning stays off") { ws =>
    // Auto-discovery is OFF (no scanForAnnotationProcessors). Mapstruct-processor is the only
    // explicit entry; Lombok is in dependencies but should NOT be auto-discovered.
    ws.yaml(
      """projects:
        |  a:
        |    dependencies:
        |      - org.projectlombok:lombok:1.18.46
        |      - org.mapstruct:mapstruct:1.5.5.Final
        |    source-layout: java
        |    platform:
        |      name: jvm
        |    java:
        |      annotationProcessors:
        |        - org.mapstruct:mapstruct-processor:1.5.5.Final
        |""".stripMargin
    )
    ws.file("a/src/java/test/User.java", "package test; public class User { public String name; public User(String name) { this.name = name; } }")
    ws.file("a/src/java/test/UserDto.java", "package test; public class UserDto { public String name; }")
    ws.file(
      "a/src/java/test/UserMapper.java",
      """package test;
        |import org.mapstruct.Mapper;
        |@Mapper
        |public interface UserMapper { UserDto userToUserDto(User user); }""".stripMargin
    )

    val (started, commands, storingLogger) = ws.start()
    val projectName = model.CrossProjectName(model.ProjectName("a"), None)
    val resolved = started.resolvedProjects(projectName).forceGet("test")

    commands.compile(List(projectName))
    val genDir = resolved.sources.find(_.toString.contains("generated-sources")).getOrElse(sys.error("expected gen-sources dir"))
    assert(Files.isRegularFile(genDir.resolve("test").resolve("UserMapperImpl.java")))
    assert(
      !storingLogger.underlying.exists(_.message.plainText.contains("auto-discovered annotation processor")),
      "scanning is off — should not have emitted any auto-discovery log line, even though lombok is in dependencies"
    )
  }

  integrationTest("scanForAnnotationProcessors: true with no processors fails loud") { ws =>
    ws.yaml(
      """projects:
        |  a:
        |    dependencies: org.slf4j:slf4j-api:2.0.9
        |    source-layout: java
        |    platform:
        |      name: jvm
        |    java:
        |      scanForAnnotationProcessors: true
        |""".stripMargin
    )
    ws.file("a/src/java/test/Person.java", "package test; public class Person {}")
    val (_, commands, _) = ws.start()
    val projectName = model.CrossProjectName(model.ProjectName("a"), None)
    val ex = intercept[Throwable](commands.compile(List(projectName)))
    val msg = Option(ex.getMessage).getOrElse("") + Option(ex.getCause).map(_.getMessage).getOrElse("")
    assert(
      msg.contains("Annotation processor resolution failed"),
      s"unexpected error message: $msg"
    )
  }
}
