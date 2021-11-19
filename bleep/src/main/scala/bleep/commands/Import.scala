package bleep.commands

import bleep.{BleepCommand, BuildPaths, Defaults, bootstrap, deduplicateBuild, importBloopFilesFromSbt}
import bleep.internal.{Os, ShortenJson}
import cats.effect.{ExitCode, IO}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

case object Import extends BleepCommand {
  override def run(): IO[ExitCode] = IO {
    val buildPaths = BuildPaths(Os.cwd / Defaults.BuildFileName)

    val build = deduplicateBuild(importBloopFilesFromSbt(bootstrap.logger, buildPaths))
    Files.writeString(
      buildPaths.bleepJsonFile,
      build.asJson.foldWith(ShortenJson).spaces2,
      UTF_8
    )
    bootstrap.logger.info(s"Imported ${build.projects.size} projects")
    ExitCode.Success
  }
}

