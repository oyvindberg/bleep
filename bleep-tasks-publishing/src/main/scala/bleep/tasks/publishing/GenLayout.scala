package bleep.tasks.publishing

import bleep.{constants, ProjectPaths, RelPath}
import coursier.core.{Configuration, Dependency}

import java.io._
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.jar.{JarEntry, JarOutputStream, Manifest}
import scala.collection.mutable
import scala.xml.Elem

object GenLayout {
  val p = new scala.xml.PrettyPrinter(120, 2)

  def ivy(
      self: Dependency,
      projectPaths: ProjectPaths,
      deps: List[Dependency],
      publication: ZonedDateTime
  ): IvyLayout[RelPath, Array[Byte]] = {
    val m = maven(self, projectPaths, deps)
    IvyLayout(
      self = self,
      jarFile = m.jarFile._2,
      sourceFile = m.sourceFile._2,
      ivyFile = fromXml(ivyFile(self, deps, publication)),
      pomFile = m.pomFile._2,
      docFile = m.docFile._2
    )
  }

  def maven(self: Dependency, projectPaths: ProjectPaths, deps: List[Dependency]): MavenLayout[RelPath, Array[Byte]] =
    MavenLayout(
      self = self,
      jarFile = createJar(Array(projectPaths.classes) ++ projectPaths.resourcesDirs.values),
      sourceFile = createJar(projectPaths.sourcesDirs.values),
      pomFile = fromXml(pomFile(self, deps)),
      // javadoc should never have existed.
      docFile = createJar(Nil)
    )

  private def fromXml(xml: Elem): Array[Byte] = {
    val prelude: String = """<?xml version="1.0" encoding="UTF-8"?>"""
    (prelude + p.format(xml)).getBytes(StandardCharsets.UTF_8)
  }

  private def createManifest(): Manifest = {
    val m = new java.util.jar.Manifest()
    m.getMainAttributes.put(java.util.jar.Attributes.Name.MANIFEST_VERSION, "1.0")
    m.getMainAttributes.putValue("Created-By", s"Bleep/${constants.version}")
    m
  }

  // adapted from mill
  def createJar(fromFolders: Iterable[Path]): Array[Byte] = {
    val seen = mutable.Set[RelPath](RelPath.force("META-INF") / "MANIFEST.MF")
    val baos = new ByteArrayOutputStream(1024 * 1024)
    val jar = new JarOutputStream(baos, createManifest())

    try
      fromFolders.foreach { fromFolder =>
        if (Files.exists(fromFolder))
          Files.walk(fromFolder).forEach { file =>
            if (Files.isRegularFile(file)) {
              val mapping = RelPath.relativeTo(fromFolder, file)
              if (!seen(mapping)) {
                seen.add(mapping)
                val entry = new JarEntry(mapping.toString)
                entry.setTime(0L)
                jar.putNextEntry(entry)
                jar.write(Files.readAllBytes(file))
                jar.closeEntry()
              }
            }
          }
      }
    finally jar.close()

    baos.toByteArray
  }

  def ivyFile(self: Dependency, deps: List[Dependency], publication: ZonedDateTime): Elem =
    <ivy-module version="2.0" xmlns:e="http://ant.apache.org/ivy/extra">
      <info organisation={self.module.organization.value}
            module={self.module.name.value}
            revision={self.version}
            status="release"
            publication={publication.format(DateTimeFormatter.ofPattern("ddMMyyyyhhmmss"))}>
        <description>
          {self.module.name.value}
        </description>
      </info>
      <configurations>
        <conf name="compile" visibility="public" description=""/>
        <conf name="runtime" visibility="public" description="" extends="compile"/>
        <conf name="test" visibility="public" description="" extends="runtime"/>
        <conf name="provided" visibility="public" description=""/>
        <conf name="optional" visibility="public" description=""/>
        <conf name="compile-internal" visibility="private" description="" extends="compile,optional,provided"/>
        <conf name="runtime-internal" visibility="private" description="" extends="runtime,optional"/>
        <conf name="test-internal" visibility="private" description="" extends="test,optional,provided"/>
        <conf name="plugin" visibility="private" description=""/>
        <conf name="pom" visibility="public" description=""/>
        <conf name="scala-tool" visibility="private" description=""/>
      </configurations>
      <publications>
        <artifact name={self.module.name.value} type="jar" ext="jar" conf="compile"/>
        <artifact name={self.module.name.value} type="pom" ext="pom" conf="pom"/>
        <artifact name={self.module.name.value} type="src" ext="jar" conf="compile" e:classifier="sources"/>
      </publications>
      <dependencies>{
      deps.map { dep =>
        <dependency 
          org={dep.module.organization.value} 
          name={dep.module.name.value} 
          rev={dep.version} 
          conf={dep.configuration.value}
          />
      }
    }
      </dependencies>
    </ivy-module>

  def pomFile(self: Dependency, dependencies: List[Dependency]): Elem =
    <project xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://maven.apache.org/POM/4.0.0">
      <modelVersion>4.0.0</modelVersion>
      <groupId>{self.module.organization.value}</groupId>
      <artifactId>{self.module.name.value}</artifactId>
      <packaging>jar</packaging>
      <description>{self.module.name.value}</description>
      <version>{self.version}</version>
      <name>{self.module.name.value}</name>
      <organization>
        <name>{self.module.organization.value}</name>
      </organization>
      <dependencies>{
      dependencies.map { dep =>
        <dependency>
          <groupId>{dep.module.organization.value}</groupId>
          <artifactId>{dep.module.name.value}</artifactId>
          <version>{dep.version}</version>{
          dep.configuration match {
            case Configuration.empty => Nil
            case other               => <scope>{other.value}</scope>

          }
        }</dependency>
      }
    }
      </dependencies>
    </project>
}
