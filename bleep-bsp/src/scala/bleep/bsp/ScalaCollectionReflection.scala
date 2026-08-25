package bleep.bsp

/** Builds Scala collections inside another classloader, and extracts their elements.
  *
  * A `TestAdapter` resolved through `CompilerResolver` brings its own `scala-library`. The adapter's methods take and return that library's `List`, `Map`, and
  * `Option`. Passing a `List` that bleep's own `scala-library` built to an adapter method raises an `IllegalArgumentException`. Every method in this object
 *     takes the classloader that owns the collections.
  */
object ScalaCollectionReflection {

  def toScalaMap(javaMap: Map[String, String], loader: ClassLoader): Any = {
    val mapCompanion = loader.loadClass("scala.collection.immutable.Map$")
    val mapObj = mapCompanion.getField("MODULE$").get(null)
    val emptyMethod = mapCompanion.getMethod("empty")
    var result = emptyMethod.invoke(mapObj)
    javaMap.foreach { case (k, v) =>
      // Each added entry produces a map of a different class: `Map$Map1` through `Map$Map4`, then `HashMap`. `getMethod` returns a `Method` that the
      // queried class declares. A `Method` declared by `Map$EmptyMap$` rejects a `Map$Map1` receiver. This lookup repeats for every entry.
      val updatedMethod = result.getClass.getMethod("updated", classOf[Object], classOf[Object])
      result = updatedMethod.invoke(result, k, v)
    }
    result
  }

  def toScalaList(javaList: List[Any], loader: ClassLoader): Any = {
    val nilClass = loader.loadClass("scala.collection.immutable.Nil$")
    val nilObj = nilClass.getField("MODULE$").get(null)
    val consClass = loader.loadClass("scala.collection.immutable.$colon$colon")
    val consConstructor = consClass.getConstructor(classOf[Object], loader.loadClass("scala.collection.immutable.List"))
    javaList.foldRight(nilObj: Any) { (elem, acc) =>
      consConstructor.newInstance(elem.asInstanceOf[AnyRef], acc.asInstanceOf[AnyRef])
    }
  }

  def fromScalaList[A](scalaList: Any, loader: ClassLoader): List[A] = {
    val result = scala.collection.mutable.ListBuffer[A]()
    var current = scalaList
    val nilClass = loader.loadClass("scala.collection.immutable.Nil$")
    val nilObj = nilClass.getField("MODULE$").get(null)
    while (current != nilObj) {
      val headMethod = current.getClass.getMethod("head")
      val tailMethod = current.getClass.getMethod("tail")
      result += headMethod.invoke(current).asInstanceOf[A]
      current = tailMethod.invoke(current)
    }
    result.toList
  }

  def fromScalaOption[A](scalaOption: Any, loader: ClassLoader): Option[A] = {
    val noneClass = loader.loadClass("scala.None$")
    val noneObj = noneClass.getField("MODULE$").get(null)
    if (scalaOption == noneObj) None
    else {
      val getMethod = scalaOption.getClass.getMethod("get")
      Some(getMethod.invoke(scalaOption).asInstanceOf[A])
    }
  }
}
