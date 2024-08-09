package bleep.packaging

import bleep.model

import java.util.jar.{Attributes, Manifest}

trait ManifestCreator {
  def apply(jarType: JarType, projectName: Option[model.CrossProjectName], mainClass: Option[String]): Manifest
}

object ManifestCreator {
  object default extends ManifestCreator {
    override def apply(jarType: JarType, projectName: Option[model.CrossProjectName], mainClass: Option[String]): Manifest = {
      val m = new java.util.jar.Manifest()
      val attrs = m.getMainAttributes
      attrs.put(java.util.jar.Attributes.Name.MANIFEST_VERSION, "1.0")
      attrs.putValue("Created-By", s"Bleep/${model.BleepVersion.current.value}")
      projectName.foreach(x => attrs.put(Attributes.Name.IMPLEMENTATION_TITLE, x.value))
      mainClass.foreach(x => attrs.put(Attributes.Name.MAIN_CLASS, x))
      m
    }
  }
}
