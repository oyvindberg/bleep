package bleep.packaging

import java.nio.file.Path

final case class Program(name: String, mainClass: String)

sealed trait PackageCommand

object PackageCommand {
  final case class Jars(projects: List[String]) extends PackageCommand
  final case class Dist(project: String, programs: List[Program], path: Option[Path]) extends PackageCommand
}
