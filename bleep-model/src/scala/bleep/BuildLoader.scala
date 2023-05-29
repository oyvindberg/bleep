package bleep

import io.circe.Json

import java.nio.file.{Files, Path}
import scala.util.control.NonFatal

sealed trait BuildLoader {
  def bleepYaml: Path
  def buildDirectory: Path = bleepYaml.getParent
  def existing: Either[BleepException, BuildLoader.Existing]

  def reloadFromDisk(): BuildLoader =
    BuildLoader.fromBleepYaml(bleepYaml)
}

object BuildLoader {
  val BuildFileName = "bleep.yaml"

  case class NonExisting(bleepYaml: Path) extends BuildLoader {
    override def existing: Either[BleepException, Existing] = Left(new BleepException.BuildNotFound(bleepYaml))
  }

  object Existing {
    def apply(bleepJson: Path): Existing = {
      val str: Lazy[Either[BleepException, String]] =
        Lazy {
          try Right(Files.readString(bleepJson))
          catch { case NonFatal(th) => Left(new BleepException.Cause(th, s"couldn't read $bleepJson")) }
        }
      Existing(bleepJson, str)
    }
  }

  case class Existing(bleepYaml: Path, str: Lazy[Either[BleepException, String]]) extends BuildLoader {
    override def existing: Either[BleepException, Existing] = Right(this)

    val json: Lazy[Either[BleepException, Json]] =
      str.map {
        case Left(be) => Left(be)
        case Right(jsonStr) =>
          try
            yaml.parse(jsonStr).left.map(th => new BleepException.InvalidJson(bleepYaml, th))
          catch {
            case NonFatal(th) => Left(new BleepException.InvalidJson(bleepYaml, th))
          }
      }

    val wantedVersion: Lazy[Either[Exception, model.BleepVersion]] =
      json.map(_.flatMap(json => json.hcursor.downField("$version").as[model.BleepVersion]))

    val buildFile: Lazy[Either[BleepException, model.BuildFile]] =
      json.map {
        case Left(be) => Left(be)
        case Right(json) =>
          json.as[model.BuildFile] match {
            case Left(th)     => Left(new BleepException.InvalidJson(bleepYaml, th))
            case Right(build) => Right(build)
          }
      }
  }

  def inDirectory(dir: Path): BuildLoader =
    fromBleepYaml(dir.resolve(BuildFileName))

  def fromBleepYaml(file: Path): BuildLoader =
    if (file.toFile.exists()) BuildLoader.Existing(file) else BuildLoader.NonExisting(file)

  def find(cwd: Path): BuildLoader = {
    // keep looking up until we find build file
    def in(dir: Path): Option[Existing] =
      inDirectory(dir) match {
        case NonExisting(_) => Option(dir.getParent).flatMap(in)
        case e: Existing    => Some(e)
      }

    in(cwd).getOrElse(nonExisting(cwd))
  }

  def nonExisting(cwd: Path): NonExisting =
    NonExisting(cwd.resolve(BuildFileName))
}
