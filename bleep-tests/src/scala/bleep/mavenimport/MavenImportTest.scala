package bleep.mavenimport

import bleep.model
import org.scalactic.TripleEqualsSupport
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}

class MavenImportTest extends AnyFunSuite with TripleEqualsSupport {

  /** The POMs below hardcode a POSIX `/tmp/test-maven` build directory, but the tests that go on to call `buildFromMavenPom` relativize it against a real
    * `Files.createTempDirectory` build dir. On Windows `Path.of("/tmp/test-maven").toAbsolutePath` is drive-relative and binds to the current drive (`D:` on
    * CI) while the temp dir lives on `C:`, so `WindowsPath.relativize` rejects the pair with `'other' has different root`. Point the POM at the actual temp dir
    * so both sides share a root on every OS. Forward slashes throughout — `Path.of("C:/x/y")` is fine on Windows, and it keeps the XML free of escapes.
    */
  private def pomIn(xml: String, tempDir: Path): String =
    xml.replace("/tmp/test-maven", tempDir.toString.replace('\\', '/'))

  test("parse single-module effective POM") {
    val xml = """<?xml version="1.0" encoding="UTF-8"?>
      |<project>
      |  <groupId>com.example</groupId>
      |  <artifactId>myapp</artifactId>
      |  <version>1.0.0</version>
      |  <packaging>jar</packaging>
      |  <build>
      |    <directory>/tmp/test-maven/target</directory>
      |    <sourceDirectory>/tmp/test-maven/src/main/java</sourceDirectory>
      |    <testSourceDirectory>/tmp/test-maven/src/test/java</testSourceDirectory>
      |    <resources>
      |      <resource><directory>/tmp/test-maven/src/main/resources</directory></resource>
      |    </resources>
      |    <testResources>
      |      <testResource><directory>/tmp/test-maven/src/test/resources</directory></testResource>
      |    </testResources>
      |    <plugins></plugins>
      |  </build>
      |  <dependencies>
      |    <dependency>
      |      <groupId>com.google.guava</groupId>
      |      <artifactId>guava</artifactId>
      |      <version>33.0.0-jre</version>
      |      <scope>compile</scope>
      |      <optional>false</optional>
      |    </dependency>
      |  </dependencies>
      |  <repositories></repositories>
      |  <modules></modules>
      |</project>""".stripMargin

    val tempFile = Files.createTempFile("effective-pom", ".xml")
    try {
      Files.writeString(tempFile, xml)
      val projects = parsePom(MavenFs.Real, tempFile)

      assert(projects.size === 1)
      val project = projects.head
      assert(project.groupId === "com.example")
      assert(project.artifactId === "myapp")
      assert(project.version === "1.0.0")
      assert(project.packaging === "jar")
      assert(project.dependencies.size === 1)
      assert(project.dependencies.head.groupId === "com.google.guava")
      assert(project.dependencies.head.artifactId === "guava")
      assert(project.dependencies.head.version === "33.0.0-jre")
      assert(project.dependencies.head.scope === "compile")
    } finally Files.deleteIfExists(tempFile)
  }

  test("parse multi-module effective POM") {
    val xml = """<?xml version="1.0" encoding="UTF-8"?>
      |<projects>
      |  <project>
      |    <groupId>com.example</groupId>
      |    <artifactId>parent</artifactId>
      |    <version>1.0.0</version>
      |    <packaging>pom</packaging>
      |    <build>
      |      <directory>/tmp/test-maven/target</directory>
      |      <plugins></plugins>
      |    </build>
      |    <dependencies></dependencies>
      |    <repositories></repositories>
      |    <modules><module>child</module></modules>
      |  </project>
      |  <project>
      |    <groupId>com.example</groupId>
      |    <artifactId>child</artifactId>
      |    <version>1.0.0</version>
      |    <packaging>jar</packaging>
      |    <build>
      |      <directory>/tmp/test-maven/child/target</directory>
      |      <sourceDirectory>/tmp/test-maven/child/src/main/java</sourceDirectory>
      |      <testSourceDirectory>/tmp/test-maven/child/src/test/java</testSourceDirectory>
      |      <plugins></plugins>
      |    </build>
      |    <dependencies>
      |      <dependency>
      |        <groupId>junit</groupId>
      |        <artifactId>junit</artifactId>
      |        <version>4.13.2</version>
      |        <scope>test</scope>
      |        <optional>false</optional>
      |      </dependency>
      |    </dependencies>
      |    <repositories></repositories>
      |    <modules></modules>
      |  </project>
      |</projects>""".stripMargin

    val tempFile = Files.createTempFile("effective-pom", ".xml")
    try {
      Files.writeString(tempFile, xml)
      val projects = parsePom(MavenFs.Real, tempFile)

      assert(projects.size === 2)
      assert(projects(0).artifactId === "parent")
      assert(projects(0).packaging === "pom")
      assert(projects(0).modules === List("child"))
      assert(projects(1).artifactId === "child")
      assert(projects(1).dependencies.size === 1)
      assert(projects(1).dependencies.head.scope === "test")
    } finally Files.deleteIfExists(tempFile)
  }

  test("parse dependency exclusions") {
    val xml = """<?xml version="1.0" encoding="UTF-8"?>
      |<project>
      |  <groupId>com.example</groupId>
      |  <artifactId>myapp</artifactId>
      |  <version>1.0.0</version>
      |  <packaging>jar</packaging>
      |  <build>
      |    <directory>/tmp/test-maven/target</directory>
      |    <plugins></plugins>
      |  </build>
      |  <dependencies>
      |    <dependency>
      |      <groupId>com.mysql</groupId>
      |      <artifactId>mysql-connector-j</artifactId>
      |      <version>9.1.0</version>
      |      <scope>compile</scope>
      |      <optional>false</optional>
      |      <exclusions>
      |        <exclusion>
      |          <groupId>com.google.protobuf</groupId>
      |          <artifactId>protobuf-java</artifactId>
      |        </exclusion>
      |      </exclusions>
      |    </dependency>
      |  </dependencies>
      |  <repositories></repositories>
      |  <modules></modules>
      |</project>""".stripMargin

    val tempFile = Files.createTempFile("effective-pom", ".xml")
    try {
      Files.writeString(tempFile, xml)
      val projects = parsePom(MavenFs.Real, tempFile)
      val dep = projects.head.dependencies.head

      assert(dep.exclusions.size === 1)
      assert(dep.exclusions.head.groupId === "com.google.protobuf")
      assert(dep.exclusions.head.artifactId === "protobuf-java")
    } finally Files.deleteIfExists(tempFile)
  }

  test("detect Kotlin version from plugin") {
    val xml = """<?xml version="1.0" encoding="UTF-8"?>
      |<project>
      |  <groupId>com.example</groupId>
      |  <artifactId>kotlin-app</artifactId>
      |  <version>1.0.0</version>
      |  <packaging>jar</packaging>
      |  <build>
      |    <directory>/tmp/test-maven/target</directory>
      |    <sourceDirectory>/tmp/test-maven/src/main/kotlin</sourceDirectory>
      |    <testSourceDirectory>/tmp/test-maven/src/test/kotlin</testSourceDirectory>
      |    <plugins>
      |      <plugin>
      |        <groupId>org.jetbrains.kotlin</groupId>
      |        <artifactId>kotlin-maven-plugin</artifactId>
      |        <version>2.1.20</version>
      |        <configuration>
      |          <jvmTarget>21</jvmTarget>
      |          <javaParameters>true</javaParameters>
      |        </configuration>
      |      </plugin>
      |    </plugins>
      |  </build>
      |  <dependencies>
      |    <dependency>
      |      <groupId>org.jetbrains.kotlin</groupId>
      |      <artifactId>kotlin-stdlib-jdk8</artifactId>
      |      <version>2.1.20</version>
      |      <scope>compile</scope>
      |      <optional>false</optional>
      |    </dependency>
      |  </dependencies>
      |  <repositories></repositories>
      |  <modules></modules>
      |</project>""".stripMargin

    val tempFile = Files.createTempFile("effective-pom", ".xml")
    val tempDir = Files.createTempDirectory("test-maven")
    try {
      Files.writeString(tempFile, pomIn(xml, tempDir))
      val mavenProjects = parsePom(MavenFs.Real, tempFile)

      val build = buildFromMavenPom(
        ryddig.Loggers.storing(),
        MavenFs.Real,
        bleep.BuildPaths(tempDir, tempDir.resolve("bleep.yaml"), model.BuildVariant.Normal, None),
        mavenProjects,
        // these poms manage no versions, so the import never asks what maven resolved
        tempDir.resolve("dependency-list.txt"),
        model.BleepVersion("1.0.0-M1"),
        buildJvm = None
      )

      val mainProject = build.explodedProjects.values.find(!_.isTestProject.contains(true))
      assert(mainProject.isDefined)
      val project = mainProject.get
      assert(project.kotlin.isDefined)
      assert(project.kotlin.get.version === Some(model.VersionKotlin("2.1.20")))
      assert(project.kotlin.get.jvmTarget === Some("21"))
      // `<javaParameters>true</javaParameters>` must survive as kotlinc's `-java-parameters`:
      // without it Jackson cannot deserialize into Kotlin data classes at runtime
      assert(project.kotlin.get.options.render.contains("-java-parameters"))
    } finally {
      Files.deleteIfExists(tempFile)
      bleep.internal.FileUtils.deleteDirectory(tempDir)
    }
  }

  test("kotlin compiler-plugin config under a compile <execution> (as maven merges inherited pluginManagement)") {
    // A module that declares kotlin-maven-plugin with <executions> gets configuration inherited from a parent's pluginManagement merged into the compile
    // execution's <configuration>, not the plugin level. The importer must read execution-level config, or all-open/-Xjvm-default/pluginOptions vanish and CDI
    // beans compile final (breaking Quarkus proxying and subclass-mocking). This is the common multi-module Quarkus+Kotlin shape.
    val xml = """<?xml version="1.0" encoding="UTF-8"?>
      |<project>
      |  <groupId>com.example</groupId>
      |  <artifactId>kotlin-quarkus-app</artifactId>
      |  <version>1.0.0</version>
      |  <packaging>jar</packaging>
      |  <build>
      |    <directory>/tmp/test-maven/target</directory>
      |    <sourceDirectory>/tmp/test-maven/src/main/kotlin</sourceDirectory>
      |    <testSourceDirectory>/tmp/test-maven/src/test/kotlin</testSourceDirectory>
      |    <plugins>
      |      <plugin>
      |        <groupId>org.jetbrains.kotlin</groupId>
      |        <artifactId>kotlin-maven-plugin</artifactId>
      |        <version>2.4.0</version>
      |        <executions>
      |          <execution>
      |            <id>compile</id>
      |            <goals><goal>compile</goal></goals>
      |            <configuration>
      |              <javaParameters>true</javaParameters>
      |              <jvmTarget>21</jvmTarget>
      |              <args><arg>-Xjvm-default=all</arg></args>
      |              <compilerPlugins><plugin>all-open</plugin></compilerPlugins>
      |              <pluginOptions>
      |                <option>all-open:annotation=jakarta.enterprise.context.ApplicationScoped</option>
      |              </pluginOptions>
      |            </configuration>
      |          </execution>
      |        </executions>
      |      </plugin>
      |    </plugins>
      |  </build>
      |  <dependencies>
      |    <dependency>
      |      <groupId>org.jetbrains.kotlin</groupId>
      |      <artifactId>kotlin-stdlib-jdk8</artifactId>
      |      <version>2.4.0</version>
      |      <scope>compile</scope>
      |      <optional>false</optional>
      |    </dependency>
      |  </dependencies>
      |  <repositories></repositories>
      |  <modules></modules>
      |</project>""".stripMargin

    val tempFile = Files.createTempFile("effective-pom", ".xml")
    val tempDir = Files.createTempDirectory("test-maven")
    try {
      Files.writeString(tempFile, pomIn(xml, tempDir))
      val mavenProjects = parsePom(MavenFs.Real, tempFile)
      val build = buildFromMavenPom(
        ryddig.Loggers.storing(),
        MavenFs.Real,
        bleep.BuildPaths(tempDir, tempDir.resolve("bleep.yaml"), model.BuildVariant.Normal, None),
        mavenProjects,
        tempDir.resolve("dependency-list.txt"),
        model.BleepVersion("1.0.0-M1"),
        buildJvm = None
      )
      val kotlin = build.explodedProjects.values.find(!_.isTestProject.contains(true)).flatMap(_.kotlin).getOrElse(sys.error("no kotlin"))
      assert(kotlin.jvmTarget === Some("21"))
      assert(kotlin.compilerPlugins.values.contains("all-open"), kotlin.compilerPlugins.values.mkString(","))
      val opts = kotlin.options.render
      // `-P` and its `plugin:...` value must render as two adjacent tokens — kotlinc and the compile server pair them positionally; a single
      // "-P plugin:..." string would make all-open silently do nothing (CDI beans compile final).
      assert(
        opts.containsSlice(List("-P", "plugin:org.jetbrains.kotlin.allopen:annotation=jakarta.enterprise.context.ApplicationScoped")),
        opts
      )
      assert(opts.contains("-Xjvm-default=all"), opts)
      assert(opts.contains("-java-parameters"), opts)
    } finally {
      Files.deleteIfExists(tempFile)
      bleep.internal.FileUtils.deleteDirectory(tempDir)
    }
  }

  test("detect Scala version from dependency") {
    val xml = """<?xml version="1.0" encoding="UTF-8"?>
      |<project>
      |  <groupId>com.example</groupId>
      |  <artifactId>scala-app</artifactId>
      |  <version>1.0.0</version>
      |  <packaging>jar</packaging>
      |  <build>
      |    <directory>/tmp/test-maven/target</directory>
      |    <sourceDirectory>/tmp/test-maven/src/main/java</sourceDirectory>
      |    <testSourceDirectory>/tmp/test-maven/src/test/java</testSourceDirectory>
      |    <plugins></plugins>
      |  </build>
      |  <dependencies>
      |    <dependency>
      |      <groupId>org.scala-lang</groupId>
      |      <artifactId>scala-library</artifactId>
      |      <version>2.13.14</version>
      |      <scope>compile</scope>
      |      <optional>false</optional>
      |    </dependency>
      |    <dependency>
      |      <groupId>org.typelevel</groupId>
      |      <artifactId>cats-core_2.13</artifactId>
      |      <version>2.10.0</version>
      |      <scope>compile</scope>
      |      <optional>false</optional>
      |    </dependency>
      |  </dependencies>
      |  <repositories></repositories>
      |  <modules></modules>
      |</project>""".stripMargin

    val tempFile = Files.createTempFile("effective-pom", ".xml")
    val tempDir = Files.createTempDirectory("test-maven")
    try {
      Files.writeString(tempFile, pomIn(xml, tempDir))
      val mavenProjects = parsePom(MavenFs.Real, tempFile)

      val build = buildFromMavenPom(
        ryddig.Loggers.storing(),
        MavenFs.Real,
        bleep.BuildPaths(tempDir, tempDir.resolve("bleep.yaml"), model.BuildVariant.Normal, None),
        mavenProjects,
        // these poms manage no versions, so the import never asks what maven resolved
        tempDir.resolve("dependency-list.txt"),
        model.BleepVersion("1.0.0-M1"),
        buildJvm = None
      )

      val mainProject = build.explodedProjects.values.find(!_.isTestProject.contains(true))
      assert(mainProject.isDefined)
      val project = mainProject.get

      // Scala version detected from scala-library dependency
      assert(project.scala.isDefined)
      assert(project.scala.get.version === Some(model.VersionScala("2.13.14")))

      // cats-core_2.13 should be converted to a ScalaDependency with base name "cats-core"
      val catsDep = project.dependencies.values.collectFirst {
        case dep: model.Dep.ScalaDependency if dep.baseModuleName.value == "cats-core" => dep
      }
      assert(catsDep.isDefined)
      assert(catsDep.get.fullCrossVersion === false)

      // scala-library should be filtered out (bleep provides it)
      val scalaLibDep = project.dependencies.values.collectFirst {
        case dep: model.Dep.JavaDependency if dep.moduleName.value == "scala-library" => dep
      }
      assert(scalaLibDep.isEmpty, "scala-library should be filtered out")
    } finally {
      Files.deleteIfExists(tempFile)
      bleep.internal.FileUtils.deleteDirectory(tempDir)
    }
  }

  test("custom repositories extracted") {
    val xml = """<?xml version="1.0" encoding="UTF-8"?>
      |<project>
      |  <groupId>com.example</groupId>
      |  <artifactId>myapp</artifactId>
      |  <version>1.0.0</version>
      |  <packaging>jar</packaging>
      |  <build>
      |    <directory>/tmp/test-maven/target</directory>
      |    <plugins></plugins>
      |  </build>
      |  <dependencies></dependencies>
      |  <repositories>
      |    <repository>
      |      <id>central</id>
      |      <url>https://repo.maven.apache.org/maven2</url>
      |    </repository>
      |    <repository>
      |      <id>spring-milestones</id>
      |      <url>https://repo.spring.io/milestone</url>
      |    </repository>
      |  </repositories>
      |  <modules></modules>
      |</project>""".stripMargin

    val tempFile = Files.createTempFile("effective-pom", ".xml")
    val tempDir = Files.createTempDirectory("test-maven")
    try {
      Files.writeString(tempFile, pomIn(xml, tempDir))
      val mavenProjects = parsePom(MavenFs.Real, tempFile)

      val build = buildFromMavenPom(
        ryddig.Loggers.storing(),
        MavenFs.Real,
        bleep.BuildPaths(tempDir, tempDir.resolve("bleep.yaml"), model.BuildVariant.Normal, None),
        mavenProjects,
        // these poms manage no versions, so the import never asks what maven resolved
        tempDir.resolve("dependency-list.txt"),
        model.BleepVersion("1.0.0-M1"),
        buildJvm = None
      )

      // Maven Central should be filtered out, Spring Milestones should remain
      val repos = build.resolvers.values
      assert(repos.size === 1)
      val repo = repos.head.asInstanceOf[model.Repository.Maven]
      assert(repo.name === Some(model.ResolverName("spring-milestones")))
    } finally {
      Files.deleteIfExists(tempFile)
      bleep.internal.FileUtils.deleteDirectory(tempDir)
    }
  }

  test("skip unresolved mainClass placeholders") {
    val xml = """<?xml version="1.0" encoding="UTF-8"?>
      |<project>
      |  <groupId>com.example</groupId>
      |  <artifactId>spring-app</artifactId>
      |  <version>1.0.0</version>
      |  <packaging>jar</packaging>
      |  <build>
      |    <directory>/tmp/test-maven/target</directory>
      |    <sourceDirectory>/tmp/test-maven/src/main/java</sourceDirectory>
      |    <testSourceDirectory>/tmp/test-maven/src/test/java</testSourceDirectory>
      |    <plugins>
      |      <plugin>
      |        <groupId>org.apache.maven.plugins</groupId>
      |        <artifactId>maven-jar-plugin</artifactId>
      |        <version>3.3.0</version>
      |        <configuration>
      |          <archive>
      |            <manifest>
      |              <mainClass>${start-class}</mainClass>
      |            </manifest>
      |          </archive>
      |        </configuration>
      |      </plugin>
      |    </plugins>
      |  </build>
      |  <dependencies></dependencies>
      |  <repositories></repositories>
      |  <modules></modules>
      |</project>""".stripMargin

    val tempFile = Files.createTempFile("effective-pom", ".xml")
    val tempDir = Files.createTempDirectory("test-maven")
    try {
      Files.writeString(tempFile, pomIn(xml, tempDir))
      val mavenProjects = parsePom(MavenFs.Real, tempFile)

      val build = buildFromMavenPom(
        ryddig.Loggers.storing(),
        MavenFs.Real,
        bleep.BuildPaths(tempDir, tempDir.resolve("bleep.yaml"), model.BuildVariant.Normal, None),
        mavenProjects,
        // these poms manage no versions, so the import never asks what maven resolved
        tempDir.resolve("dependency-list.txt"),
        model.BleepVersion("1.0.0-M1"),
        buildJvm = None
      )

      val mainProject = build.explodedProjects.values.head
      val platform = mainProject.platform.get
      assert(platform.mainClass.isEmpty, "Unresolved ${start-class} should be filtered out")
    } finally {
      Files.deleteIfExists(tempFile)
      bleep.internal.FileUtils.deleteDirectory(tempDir)
    }
  }

  test("wire quarkus test projects: template, sourcegen, scripts project") {
    val xml = """<?xml version="1.0" encoding="UTF-8"?>
      |<project>
      |  <groupId>com.example</groupId>
      |  <artifactId>quarkus-app</artifactId>
      |  <version>1.0.0</version>
      |  <packaging>jar</packaging>
      |  <build>
      |    <directory>/tmp/test-maven/target</directory>
      |    <sourceDirectory>/tmp/test-maven/src/main/java</sourceDirectory>
      |    <testSourceDirectory>/tmp/test-maven/src/test/java</testSourceDirectory>
      |    <plugins></plugins>
      |  </build>
      |  <dependencies>
      |    <dependency>
      |      <groupId>io.quarkus</groupId>
      |      <artifactId>quarkus-rest</artifactId>
      |      <version>3.15.1</version>
      |      <scope>compile</scope>
      |      <optional>false</optional>
      |    </dependency>
      |    <dependency>
      |      <groupId>io.quarkus</groupId>
      |      <artifactId>quarkus-junit5</artifactId>
      |      <version>3.15.1</version>
      |      <scope>test</scope>
      |      <optional>false</optional>
      |    </dependency>
      |  </dependencies>
      |  <repositories></repositories>
      |  <modules></modules>
      |</project>""".stripMargin

    val tempFile = Files.createTempFile("effective-pom", ".xml")
    val tempDir = Files.createTempDirectory("test-maven")
    try {
      // the -test project is only created when test sources exist
      val testSrc = tempDir.resolve("src/test/java/com/example")
      Files.createDirectories(testSrc)
      Files.writeString(testSrc.resolve("GreetingResourceTest.java"), "package com.example;\nclass GreetingResourceTest {}\n")

      Files.writeString(tempFile, pomIn(xml, tempDir))
      val mavenProjects = parsePom(MavenFs.Real, tempFile)

      val files = generateBuildFromMaven(
        bleep.BuildPaths(tempDir, tempDir.resolve("bleep.yaml"), model.BuildVariant.Normal, None),
        ryddig.Loggers.storing(),
        MavenImportOptions(
          ignoreWhenInferringTemplates = Set.empty,
          skipMvn = true,
          skipGeneratedResourcesScript = false,
          mvnPath = None,
          filtering = bleep.sbtimport.ImportFiltering.empty,
          buildJvm = None
        ),
        model.BleepVersion("1.0.0-M1"),
        model.BleepVersion("1.0.0-M1"),
        MavenFs.Real,
        mavenProjects,
        // these poms manage no versions, so the import never asks what maven resolved
        tempDir.resolve("dependency-list.txt")
      )

      val yamlString = files(tempDir.resolve("bleep.yaml"))
      val buildFile = bleep.yaml.decode[model.BuildFile](yamlString).fold(e => throw e, identity)

      val templateId = model.TemplateId("template-quarkus-test")
      val template = buildFile.templates.value.getOrElse(templateId, sys.error(s"expected $templateId in generated build"))
      assert(template.maxConcurrentSuites === Some(1))
      assert(template.testFork === Some(model.TestForkMode.PerProject))
      assert(template.sourcegen.values.exists { case model.ScriptDef.Main(project, main, _, _, _) =>
        project.name.value === "scripts" && main === "bleep.plugin.quarkus.QuarkusTestModelGen"
      })
      // No platform block: the sourcegen declares the fork's JVM options at build time by writing them to the project's forkJvmOptions file.
      assert(template.platform.isEmpty)

      val testProject = buildFile.projects.value(model.ProjectName("quarkus-app-test"))
      assert(testProject.`extends`.values.contains(templateId))

      val scriptsProject = buildFile.projects.value(model.ProjectName("scripts"))
      assert(scriptsProject.dependencies.values.exists(dep => dep.organization.value == "build.bleep" && dep.baseModuleName.value == "bleep-plugin-quarkus"))

      // dev-mode and packaging entry points are registered as scripts, each pointing at bleep-plugin-quarkus' main in the scripts project
      def scriptMain(name: String): String =
        buildFile.scripts.value
          .getOrElse(model.ScriptName(name), sys.error(s"expected $name script in generated build"))
          .values match {
          case (m: model.ScriptDef.Main) :: Nil if m.project.name.value == "scripts" => m.main
          case other                                                                 => sys.error(s"unexpected $name script: $other")
        }
      assert(scriptMain("quarkus-dev") === "bleep.plugin.quarkus.QuarkusRun")
      assert(scriptMain("quarkus-package") === "bleep.plugin.quarkus.QuarkusPackage")
    } finally {
      Files.deleteIfExists(tempFile)
      bleep.internal.FileUtils.deleteDirectory(tempDir)
    }
  }
}
