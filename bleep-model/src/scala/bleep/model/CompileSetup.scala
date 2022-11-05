package bleep.model

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

case class CompileSetup(
    order: Option[CompileOrder],
    addLibraryToBootClasspath: Option[Boolean],
    addCompilerToClasspath: Option[Boolean],
    addExtraJarsToClasspath: Option[Boolean],
    manageBootClasspath: Option[Boolean],
    filterLibraryFromClasspath: Option[Boolean]
) extends SetLike[CompileSetup] {

  override def intersect(other: CompileSetup): CompileSetup =
    CompileSetup(
      order = if (order == other.order) order else None,
      addLibraryToBootClasspath = if (addLibraryToBootClasspath == other.addLibraryToBootClasspath) addLibraryToBootClasspath else None,
      addCompilerToClasspath = if (addCompilerToClasspath == other.addCompilerToClasspath) addCompilerToClasspath else None,
      addExtraJarsToClasspath = if (addExtraJarsToClasspath == other.addExtraJarsToClasspath) addExtraJarsToClasspath else None,
      manageBootClasspath = if (manageBootClasspath == other.manageBootClasspath) manageBootClasspath else None,
      filterLibraryFromClasspath = if (filterLibraryFromClasspath == other.filterLibraryFromClasspath) filterLibraryFromClasspath else None
    )

  override def removeAll(other: CompileSetup): CompileSetup =
    CompileSetup(
      order = if (order == other.order) None else order,
      addLibraryToBootClasspath = if (addLibraryToBootClasspath == other.addLibraryToBootClasspath) None else addLibraryToBootClasspath,
      addCompilerToClasspath = if (addCompilerToClasspath == other.addCompilerToClasspath) None else addCompilerToClasspath,
      addExtraJarsToClasspath = if (addExtraJarsToClasspath == other.addExtraJarsToClasspath) None else addExtraJarsToClasspath,
      manageBootClasspath = if (manageBootClasspath == other.manageBootClasspath) None else manageBootClasspath,
      filterLibraryFromClasspath = if (filterLibraryFromClasspath == other.filterLibraryFromClasspath) None else filterLibraryFromClasspath
    )

  override def union(other: CompileSetup): CompileSetup =
    CompileSetup(
      order = order.orElse(other.order),
      addLibraryToBootClasspath = addLibraryToBootClasspath.orElse(other.addLibraryToBootClasspath),
      addCompilerToClasspath = addCompilerToClasspath.orElse(other.addCompilerToClasspath),
      addExtraJarsToClasspath = addExtraJarsToClasspath.orElse(other.addExtraJarsToClasspath),
      manageBootClasspath = manageBootClasspath.orElse(other.manageBootClasspath),
      filterLibraryFromClasspath = filterLibraryFromClasspath.orElse(other.filterLibraryFromClasspath)
    )

  override def isEmpty: Boolean = this match {
    case CompileSetup(order, addLibraryToBootClasspath, addCompilerToClasspath, addExtraJarsToClasspath, manageBootClasspath, filterLibraryFromClasspath) =>
      order.isEmpty && addLibraryToBootClasspath.isEmpty && addCompilerToClasspath.isEmpty && addExtraJarsToClasspath.isEmpty && manageBootClasspath.isEmpty && filterLibraryFromClasspath.isEmpty
  }
}

object CompileSetup {
  val empty: CompileSetup = CompileSetup(None, None, None, None, None, None)
  implicit val decodes: Decoder[CompileSetup] = deriveDecoder
  implicit val encodes: Encoder[CompileSetup] = deriveEncoder
}
