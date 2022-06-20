package bleep

import bleep.internal.{FileUtils, Lazy}
import io.circe.{yaml, Json}

import java.nio.file.{Files, Path}
import scala.util.control.NonFatal

sealed trait BuildLoader {
  def bleepJson: Path
  def buildDirectory: Path = bleepJson.getParent
  def existing: Either[BuildException, BuildLoader.Existing]
}

object BuildLoader {
  case class NonExisting(bleepJson: Path) extends BuildLoader {
    override def existing: Either[BuildException, Existing] = Left(new BuildException.BuildNotFound(bleepJson))
  }

  case class Existing(bleepJson: Path) extends BuildLoader {
    override def existing: Either[BuildException, Existing] = Right(this)

    val str: Lazy[Either[BuildException, String]] =
      Lazy {
        try Right(Files.readString(bleepJson))
        catch { case NonFatal(th) => Left(new BuildException.Cause(th, s"couldn't read $bleepJson")) }
      }

    val json: Lazy[Either[BuildException, Json]] =
      str.map {
        case Left(be) => Left(be)
        case Right(jsonStr) =>
          yaml.parser.parse(jsonStr) match {
            case Left(th)    => Left(new BuildException.InvalidJson(bleepJson, th))
            case Right(json) => Right(json)
          }
      }

    val build: Lazy[Either[BuildException, model.Build]] =
      json.map {
        case Left(be) => Left(be)
        case Right(json) =>
          json.as[model.Build] match {
            case Left(th)     => Left(new BuildException.InvalidJson(bleepJson, th))
            case Right(build) => Right(build)
          }
      }
  }

  def inDirectory(dir: Path): BuildLoader = {
    val file = dir / constants.BuildFileName
    val fileYaml = dir / constants.BuildFileNameYaml
    if (FileUtils.exists(fileYaml))
      BuildLoader.Existing(fileYaml)
    else if (FileUtils.exists(file))
      BuildLoader.Existing(file)
    else
      BuildLoader.NonExisting(file)
  }

  def find(cwd: Path): BuildLoader = {
    // keep looking up until we find build file
    def in(dir: Path): Option[Existing] =
      inDirectory(dir) match {
        case NonExisting(_) => Option(dir.getParent).flatMap(in)
        case e: Existing    => Some(e)
      }

    in(cwd).getOrElse(NonExisting(cwd / constants.BuildFileName))
  }
}
