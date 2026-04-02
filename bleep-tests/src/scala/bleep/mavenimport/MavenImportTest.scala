package bleep.mavenimport

import bleep.model
import org.scalactic.TripleEqualsSupport
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}

class MavenImportTest extends AnyFunSuite with TripleEqualsSupport {

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
      val projects = parsePom(tempFile)

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
      val projects = parsePom(tempFile)

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
      val projects = parsePom(tempFile)
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
      Files.writeString(tempFile, xml)
      val mavenProjects = parsePom(tempFile)

      val build = buildFromMavenPom(
        ryddig.Loggers.storing(),
        bleep.BuildPaths(tempDir, tempDir.resolve("bleep.yaml"), model.BuildVariant.Normal, None),
        mavenProjects,
        model.BleepVersion("1.0.0-M1")
      )

      val mainProject = build.explodedProjects.values.find(!_.isTestProject.contains(true))
      assert(mainProject.isDefined)
      val project = mainProject.get
      assert(project.kotlin.isDefined)
      assert(project.kotlin.get.version === Some(model.VersionKotlin("2.1.20")))
      assert(project.kotlin.get.jvmTarget === Some("21"))
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
      Files.writeString(tempFile, xml)
      val mavenProjects = parsePom(tempFile)

      val build = buildFromMavenPom(
        ryddig.Loggers.storing(),
        bleep.BuildPaths(tempDir, tempDir.resolve("bleep.yaml"), model.BuildVariant.Normal, None),
        mavenProjects,
        model.BleepVersion("1.0.0-M1")
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
      Files.writeString(tempFile, xml)
      val mavenProjects = parsePom(tempFile)

      val build = buildFromMavenPom(
        ryddig.Loggers.storing(),
        bleep.BuildPaths(tempDir, tempDir.resolve("bleep.yaml"), model.BuildVariant.Normal, None),
        mavenProjects,
        model.BleepVersion("1.0.0-M1")
      )

      // Maven Central should be filtered out, Spring Milestones should remain
      val repos = build.resolvers.values
      assert(repos.size === 1)
      val repo = repos.head.asInstanceOf[model.Repository.Maven]
      assert(repo.name === Some("spring-milestones"))
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
      Files.writeString(tempFile, xml)
      val mavenProjects = parsePom(tempFile)

      val build = buildFromMavenPom(
        ryddig.Loggers.storing(),
        bleep.BuildPaths(tempDir, tempDir.resolve("bleep.yaml"), model.BuildVariant.Normal, None),
        mavenProjects,
        model.BleepVersion("1.0.0-M1")
      )

      val mainProject = build.explodedProjects.values.head
      val platform = mainProject.platform.get
      assert(platform.mainClass.isEmpty, "Unresolved ${start-class} should be filtered out")
    } finally {
      Files.deleteIfExists(tempFile)
      bleep.internal.FileUtils.deleteDirectory(tempDir)
    }
  }
}
