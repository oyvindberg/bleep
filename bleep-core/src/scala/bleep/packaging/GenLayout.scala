package bleep
package packaging

import coursier.core.{Configuration, Dependency, Info}

import java.nio.charset.StandardCharsets
import scala.xml.{Elem, NodeSeq}

object GenLayout {
  val p = new scala.xml.PrettyPrinter(120, 2)

  def ivy(
      manifestCreator: ManifestCreator,
      projectName: model.CrossProjectName,
      self: Dependency,
      projectPaths: ProjectPaths,
      deps: List[Dependency],
      mainClass: Option[String]
  ): IvyLayout[RelPath, Array[Byte]] = {
    val m = maven(manifestCreator, projectName, self, projectPaths, deps, Info.empty, mainClass)
    IvyLayout(
      self = self,
      jarFile = m.jarFile._2,
      sourceFile = m.sourceFile._2,
      ivyFile = fromXml(ivyFile(self, deps)),
      pomFile = m.pomFile._2,
      docFile = m.docFile._2
    )
  }

  def maven(
      manifestCreator: ManifestCreator,
      projectName: model.CrossProjectName,
      self: Dependency,
      projectPaths: ProjectPaths,
      deps: List[Dependency],
      info: Info,
      mainClass: Option[String]
  ): MavenLayout[RelPath, Array[Byte]] =
    MavenLayout(
      self = self,
      jarFile = createJar(
        JarType.Jar,
        manifestCreator,
        Array(projectPaths.classes) ++ projectPaths.resourcesDirs.all,
        projectName = Some(projectName),
        mainClass = mainClass
      ),
      sourceFile = createJar(JarType.SourcesJar, manifestCreator, projectPaths.sourcesDirs.all, projectName = Some(projectName)),
      pomFile = fromXml(pomFile(self, deps, info)),
      // javadoc should never have existed.
      docFile = createJar(JarType.DocsJar, manifestCreator, Nil, projectName = Some(projectName))
    )

  def fromXml(xml: Elem): Array[Byte] = {
    val prelude: String = """<?xml version="1.0" encoding="UTF-8"?>"""
    (prelude + p.format(xml)).getBytes(StandardCharsets.UTF_8)
  }

  def ivyFile(self: Dependency, deps: List[Dependency]): Elem =
    <ivy-module version="2.0" xmlns:e="http://ant.apache.org/ivy/extra">
      <info organisation={self.module.organization.value}
            module={self.module.name.value}
            revision={self.version}
            status="release">
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

  implicit class OptionOps[T](ot: Option[T]) {
    def render(asXml: T => Elem): NodeSeq =
      ot match {
        case Some(t) => asXml(t)
        case None    => NodeSeq.Empty
      }
  }

  implicit class SeqOpts[T](ts: Seq[T]) {
    def render(asXml: Seq[T] => Elem): NodeSeq =
      if (ts.isEmpty) NodeSeq.Empty else asXml(ts)
  }

  def pomFile(self: Dependency, dependencies: List[Dependency], info: Info): Elem =
    <project xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://maven.apache.org/POM/4.0.0">
      <modelVersion>4.0.0</modelVersion>
      <groupId>{self.module.organization.value}</groupId>
      <artifactId>{self.module.name.value}</artifactId>
      <packaging>jar</packaging>
      <description>{info.description}</description>
      <url>{info.homePage}</url>
      <version>{self.version}</version>
      {
      info.licenseInfo.render { ls =>
        <licenses>
        {
          ls.map(l => <license>
          <name>{l.name}</name>
          {l.url.render(x => <url>{x}</url>)}
          {l.distribution.render(x => <distribution>{x}</distribution>)}
          {l.comments.render(x => <comments>{x}</comments>)}
        </license>)
        }
      </licenses>
      }
    }
      <name>{self.module.name.value}</name>
      <organization>
        <name>{self.module.organization.value}</name>
        <url>{info.homePage}</url>
      </organization>
      {
      info.scm.render { scm =>
        <scm>
          {scm.url.render(url => <url>{url}</url>)}
          {scm.connection.render(c => <connection>{c}</connection>)}
          {scm.developerConnection.render(c => <developerConnection>{c}</developerConnection>)}
        </scm>
      }
    }
    {
      info.developers.render { ds =>
        <developers>{
          ds.map { d =>
            <developer>
              <id>{d.id}</id>
              <name>{d.name}</name>
              <url>{d.url}</url>
            </developer>
          }
        }
      </developers>
      }
    }
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
        }
          {
          dep.minimizedExclusions.toSeq().render { exc =>
            <exclusions>{
              exc.map { case (org, thing) =>
                <exclusion>
                  <groupId>{org.value}</groupId>
                  <artifactId>{thing.value}</artifactId>
                </exclusion>
              }
            }</exclusions>
          }
        }</dependency>
      }
    }
      </dependencies>
    </project>
}
