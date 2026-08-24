package bleep.bsp

/** Builds and reads Scala collections that live in another classloader.
  *
  * A `TestAdapter` resolved through `CompilerResolver` brings its own `scala-library`, and its methods take and return that library's `List`, `Map`, and
  * `Option`. A `List` built here would come from bleep's own `scala-library` and fail with an `IllegalArgumentException` at the call. Every method below
  *  takes the classloader whose collections it is working with.
  */
object ScalaCollectionReflection {

  def toScalaMap(javaMap: Map[String, String], loader: ClassLoader): Any = {
    val mapCompanion = loader.loadClass("scala.collection.immutable.Map$")
    val mapObj = mapCompanion.getField("MODULE$").get(null)
    val emptyMethod = mapCompanion.getMethod("empty")
    var result = emptyMethod.invoke(mapObj)
    val updatedMethod = result.getClass.getMethod("updated", classOf[Object], classOf[Object])
    javaMap.foreach { case (k, v) =>
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
