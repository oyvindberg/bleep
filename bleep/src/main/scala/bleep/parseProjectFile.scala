package bleep

import io.circe.parser

object parseProjectFile {
  def apply(json: String): model.File =
    parser.decode[model.File](json) match {
      case Left(error) => throw error
      case Right(file) => file
    }
}
