package bleep.internal

import bleep.model
import bloop.config.Config

object conversions {
  trait Bijection[T1, T2] {
    def to(t1: T1): T2

    def from(t2: T2): T1
  }

  object compileOrder extends Bijection[Config.CompileOrder, model.CompileOrder] {
    override def to(t1: Config.CompileOrder): model.CompileOrder = t1 match {
      case Config.Mixed         => model.CompileOrder.Mixed
      case Config.JavaThenScala => model.CompileOrder.JavaThenScala
      case Config.ScalaThenJava => model.CompileOrder.ScalaThenJava
    }

    override def from(t2: model.CompileOrder): Config.CompileOrder = t2 match {
      case model.CompileOrder.Mixed         => Config.Mixed
      case model.CompileOrder.JavaThenScala => Config.JavaThenScala
      case model.CompileOrder.ScalaThenJava => Config.ScalaThenJava
    }
  }

  object linkerMode extends Bijection[Config.LinkerMode, model.LinkerMode] {
    override def to(t1: Config.LinkerMode): model.LinkerMode = t1 match {
      case Config.LinkerMode.Debug   => model.LinkerMode.Debug
      case Config.LinkerMode.Release => model.LinkerMode.Release
    }

    override def from(t2: model.LinkerMode): Config.LinkerMode = t2 match {
      case model.LinkerMode.Debug   => Config.LinkerMode.Debug
      case model.LinkerMode.Release => Config.LinkerMode.Release
    }
  }

  object moduleKindJS extends Bijection[Config.ModuleKindJS, model.ModuleKindJS] {
    override def to(t1: Config.ModuleKindJS): model.ModuleKindJS = t1 match {
      case Config.ModuleKindJS.NoModule       => model.ModuleKindJS.NoModule
      case Config.ModuleKindJS.CommonJSModule => model.ModuleKindJS.CommonJSModule
      case Config.ModuleKindJS.ESModule       => model.ModuleKindJS.ESModule
    }

    override def from(t2: model.ModuleKindJS): Config.ModuleKindJS = t2 match {
      case model.ModuleKindJS.NoModule       => Config.ModuleKindJS.NoModule
      case model.ModuleKindJS.CommonJSModule => Config.ModuleKindJS.CommonJSModule
      case model.ModuleKindJS.ESModule       => Config.ModuleKindJS.ESModule
    }
  }
}
