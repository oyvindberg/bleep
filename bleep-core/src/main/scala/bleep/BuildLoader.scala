package bleep

import bleep.internal.FileUtils
import io.circe.Json
import io.circe.yaml12

import java.nio.file.{Files, Path}
import scala.util.control.NonFatal

sealed trait BuildLoader {
  def bleepYaml: Path
  def buildDirectory: Path = bleepYaml.getParent
  def existing: Either[BuildException, BuildLoader.Existing]
}

object BuildLoader {
  case class NonExisting(bleepYaml: Path) extends BuildLoader {
    override def existing: Either[BuildException, Existing] = Left(new BuildException.BuildNotFound(bleepYaml))
  }

  object Existing {
    def apply(bleepJson: Path): Existing = {
      val str: Lazy[Either[BuildException, String]] =
        Lazy {
          try Right(Files.readString(bleepJson))
          catch { case NonFatal(th) => Left(new BuildException.Cause(th, s"couldn't read $bleepJson")) }
        }
      Existing(bleepJson, str)
    }
  }

  case class Existing(bleepYaml: Path, str: Lazy[Either[BuildException, String]]) extends BuildLoader {
    override def existing: Either[BuildException, Existing] = Right(this)

    val json: Lazy[Either[BuildException, Json]] =
      str.map {
        case Left(be) => Left(be)
        case Right(jsonStr) =>
          try
            yaml12.parser.parse(jsonStr).left.map(th => new BuildException.InvalidJson(bleepYaml, th))
          catch {
            case NonFatal(th) => Left(new BuildException.InvalidJson(bleepYaml, th))
          }
      }

    val build: Lazy[Either[BuildException, model.Build]] =
      json.map {
        case Left(be) => Left(be)
        case Right(json) =>
          json.as[model.Build] match {
            case Left(th)     => Left(new BuildException.InvalidJson(bleepYaml, th))
            case Right(build) => Right(build)
          }
      }
  }

  def inDirectory(dir: Path): BuildLoader = {
    val file = dir / constants.BuildFileName
    if (FileUtils.exists(file))
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
