package bleep.internal

import bleep.model
import bleep.model.{JsonMap, JsonSet}

object Functions {
  def stripExtends(p: model.Project): model.Project =
    p.copy(
      `extends` = JsonSet.empty,
      cross = JsonMap(p.cross.value.map { case (n, p) => (n, stripExtends(p)) }.filterNot { case (_, p) => p.isEmpty })
    )
}
