package bleep.internal

import coursier.core.Project
import coursier.core.compatibility.xmlParseSax
import coursier.maven.PomParser

import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap

class CachingPomReader private (cache: java.util.concurrent.ConcurrentMap[Path, Project]) {
  def apply(pomPath: Path): Either[String, Project] =
    if (cache.containsKey(pomPath)) Right(cache.get(pomPath))
    else {
      if (pomPath.toFile.exists()) {
        val errOrProject = xmlParseSax(Files.readString(pomPath), new PomParser).project
        errOrProject.foreach { project =>
          cache.put(pomPath, project)
        }
        errOrProject
      } else Left(s"$pomPath doesn't exist")
    }
}
object CachingPomReader {
  def apply(): CachingPomReader = new CachingPomReader(new ConcurrentHashMap[Path, Project](500))
}
