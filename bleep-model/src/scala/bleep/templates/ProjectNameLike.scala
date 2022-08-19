package bleep
package templates

// It's overkill surely, but this let's us abstract over `model.ProjectName` and `model.CrossProjectName`
trait ProjectNameLike[P] {
  def projectName(p: P): model.ProjectName
  def asString(p: P): String
}

object ProjectNameLike {
  implicit class Syntax[P](private val p: P) extends AnyVal {
    def extractProjectName(implicit N: ProjectNameLike[P]): model.ProjectName = N.projectName(p)
    def asString(implicit N: ProjectNameLike[P]): String = N.asString(p)
  }

  implicit val projectNameName: ProjectNameLike[model.ProjectName] = new ProjectNameLike[model.ProjectName] {
    override def projectName(p: model.ProjectName): model.ProjectName = p
    override def asString(p: model.ProjectName): String = p.value
  }

  implicit val crossProjectName: ProjectNameLike[model.CrossProjectName] = new ProjectNameLike[model.CrossProjectName] {
    override def projectName(p: model.CrossProjectName): model.ProjectName = p.name
    override def asString(p: model.CrossProjectName): String = p.value
  }
}
