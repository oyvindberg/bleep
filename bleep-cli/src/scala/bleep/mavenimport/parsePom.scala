package bleep
package mavenimport

import java.nio.file.Path

object parsePom {

  def apply(effectivePomPath: Path): List[MavenProject] = {
    val xml = scala.xml.XML.loadFile(effectivePomPath.toFile)

    xml.label match {
      case "projects" =>
        // Multi-module: effective-pom wraps multiple <project> elements
        (xml \ "project").iterator.map(parseProject).toList
      case "project" =>
        // Single project
        List(parseProject(xml))
      case other =>
        throw new BleepException.Text(s"Unexpected root element in effective POM: $other")
    }
  }

  private def parseProject(node: scala.xml.Node): MavenProject = {
    val groupId = textOrEmpty(node, "groupId")
    val artifactId = textOrEmpty(node, "artifactId")
    val version = textOrEmpty(node, "version")
    val packaging = textOrDefault(node, "packaging", "jar")

    val build = node \ "build"

    // <build><directory> is the target/output directory (e.g. /path/to/project/target).
    // The project basedir is its parent.
    val buildDirectory = textOrEmpty(build, "directory")
    val directory =
      if (buildDirectory.nonEmpty) realPath(Path.of(buildDirectory).getParent)
      else realPath(Path.of("."))

    val sourceDirectory = pathOrDefault(build, "sourceDirectory", directory.resolve("src/main/java"))
    val testSourceDirectory = pathOrDefault(build, "testSourceDirectory", directory.resolve("src/test/java"))

    // Extract additional source dirs from build-helper-maven-plugin
    val allPlugins = build \ "plugins" \ "plugin"
    val (additionalSources, additionalTestSources) = parseBuildHelperSources(allPlugins, directory)

    val resources = parseResourceDirs(build \ "resources" \ "resource")
    val testResources = parseResourceDirs(build \ "testResources" \ "testResource")

    val dependencies = parseDependencies(node \ "dependencies" \ "dependency")
    val plugins = parsePlugins(allPlugins)
    val repositories = parseRepositories(node \ "repositories" \ "repository")
    val modules = (node \ "modules" \ "module").iterator.map(_.text.trim).toList

    MavenProject(
      groupId = groupId,
      artifactId = artifactId,
      version = version,
      packaging = packaging,
      directory = directory,
      sourceDirectory = sourceDirectory,
      testSourceDirectory = testSourceDirectory,
      additionalSources = additionalSources,
      additionalTestSources = additionalTestSources,
      resources = resources,
      testResources = testResources,
      dependencies = dependencies,
      plugins = plugins,
      repositories = repositories,
      modules = modules
    )
  }

  private def parseDependencies(nodes: scala.xml.NodeSeq): List[MavenDependency] =
    nodes.iterator.map { dep =>
      val exclusions = (dep \ "exclusions" \ "exclusion").iterator.map { exc =>
        MavenExclusion(
          groupId = textOrEmpty(exc, "groupId"),
          artifactId = textOrEmpty(exc, "artifactId")
        )
      }.toList

      MavenDependency(
        groupId = textOrEmpty(dep, "groupId"),
        artifactId = textOrEmpty(dep, "artifactId"),
        version = textOrEmpty(dep, "version"),
        scope = textOrDefault(dep, "scope", "compile"),
        optional = textOrDefault(dep, "optional", "false") == "true",
        exclusions = exclusions
      )
    }.toList

  private def parsePlugins(nodes: scala.xml.NodeSeq): List[MavenPlugin] =
    nodes.iterator.map { plugin =>
      MavenPlugin(
        groupId = textOrDefault(plugin, "groupId", "org.apache.maven.plugins"),
        artifactId = textOrEmpty(plugin, "artifactId"),
        version = textOrEmpty(plugin, "version"),
        configuration = plugin \ "configuration"
      )
    }.toList

  private def parseRepositories(nodes: scala.xml.NodeSeq): List[MavenRepository] =
    nodes.iterator.map { repo =>
      MavenRepository(
        id = textOrEmpty(repo, "id"),
        url = textOrEmpty(repo, "url")
      )
    }.toList

  /** Extract additional source directories from build-helper-maven-plugin executions.
    *
    * Handles both `add-source` (main) and `add-test-source` (test) goals.
    * Source paths may be relative (resolved against module directory) or absolute.
    */
  private def parseBuildHelperSources(pluginNodes: scala.xml.NodeSeq, moduleDir: Path): (List[Path], List[Path]) = {
    val mainSources = List.newBuilder[Path]
    val testSources = List.newBuilder[Path]

    pluginNodes.foreach { plugin =>
      val artifactId = textOrEmpty(plugin, "artifactId")
      if (artifactId == "build-helper-maven-plugin") {
        (plugin \ "executions" \ "execution").foreach { execution =>
          val goals = (execution \ "goals" \ "goal").map(_.text.trim)
          val sources = (execution \ "configuration" \ "sources" \ "source").map(_.text.trim)
          val resolvedPaths = sources.map { src =>
            val path = Path.of(src)
            val resolved = if (path.isAbsolute) path else moduleDir.resolve(path)
            realPath(resolved)
          }.toList

          if (goals.contains("add-source")) mainSources ++= resolvedPaths
          else if (goals.contains("add-test-source")) testSources ++= resolvedPaths
        }
      }
    }

    (mainSources.result(), testSources.result())
  }

  private def parseResourceDirs(nodes: scala.xml.NodeSeq): List[Path] =
    nodes.iterator.map(r => realPath(Path.of(textOrEmpty(r, "directory")))).toList

  /** Resolve symlinks to real path (handles macOS /tmp -> /private/tmp etc.) */
  private def realPath(path: Path): Path =
    if (path.toFile.exists()) path.toRealPath()
    else path.toAbsolutePath.normalize()

  private def pathOrDefault(parent: scala.xml.NodeSeq, childName: String, default: Path): Path = {
    val text = textOrEmpty(parent, childName)
    if (text.isEmpty) default
    else realPath(Path.of(text))
  }

  private def textOrEmpty(parent: scala.xml.NodeSeq, childName: String): String = {
    val children = parent \ childName
    if (children.isEmpty) "" else children.head.text.trim
  }

  private def textOrDefault(parent: scala.xml.NodeSeq, childName: String, default: String): String = {
    val children = parent \ childName
    if (children.isEmpty) default
    else {
      val text = children.head.text.trim
      if (text.isEmpty) default else text
    }
  }
}
