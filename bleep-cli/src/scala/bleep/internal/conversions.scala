package bleep.internal

import bleep.model
import bloop.config.Config
import bloop.config.Config.ModuleSplitStyleJS.{FewestModules, SmallModulesFor, SmallestModules}
import bloop.config.Config.NativeBuildTarget.{Application, LibraryDynamic, LibraryStatic}
import bloop.config.Config.NativeLinkerReleaseMode.ReleaseFast
import bloop.config.Config.NativeLinkerReleaseMode.ReleaseFull
import bloop.config.Config.NativeLinkerReleaseMode.ReleaseSize
import bloop.config.Config.NativeLTO.Full
import bloop.config.Config.NativeLTO.Thin

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

  object moduleSplitStyleJS extends Bijection[Config.ModuleSplitStyleJS, model.ModuleSplitStyleJS] {

    override def to(t1: Config.ModuleSplitStyleJS): model.ModuleSplitStyleJS = t1 match {
      case FewestModules             => model.ModuleSplitStyleJS.FewestModules
      case SmallModulesFor(packages) => model.ModuleSplitStyleJS.SmallModulesFor(packages)
      case SmallestModules           => model.ModuleSplitStyleJS.SmallestModules

    }

    override def from(t2: model.ModuleSplitStyleJS): Config.ModuleSplitStyleJS = t2 match {
      case model.ModuleSplitStyleJS.FewestModules             => Config.ModuleSplitStyleJS.FewestModules
      case model.ModuleSplitStyleJS.SmallModulesFor(packages) => Config.ModuleSplitStyleJS.SmallModulesFor(packages)
      case model.ModuleSplitStyleJS.SmallestModules           => Config.ModuleSplitStyleJS.SmallestModules
    }

  }

  object nativeBuildTarget extends Bijection[Config.NativeBuildTarget, model.NativeBuildTarget] {

    override def to(t1: Config.NativeBuildTarget): model.NativeBuildTarget = t1 match {
      case Application    => model.NativeBuildTarget.Application
      case LibraryDynamic => model.NativeBuildTarget.LibraryDynamic
      case LibraryStatic  => model.NativeBuildTarget.LibraryStatic
    }

    override def from(t2: model.NativeBuildTarget): Config.NativeBuildTarget = t2 match {
      case model.NativeBuildTarget.Application    => Config.NativeBuildTarget.Application
      case model.NativeBuildTarget.LibraryDynamic => Config.NativeBuildTarget.LibraryDynamic
      case model.NativeBuildTarget.LibraryStatic  => Config.NativeBuildTarget.LibraryStatic
    }

  }

  object nativeLinkerReleaseMode extends Bijection[Config.NativeLinkerReleaseMode, model.NativeLinkerReleaseMode] {

    override def to(t1: Config.NativeLinkerReleaseMode): model.NativeLinkerReleaseMode = t1 match {
      case ReleaseFast => model.NativeLinkerReleaseMode.ReleaseFast
      case ReleaseFull => model.NativeLinkerReleaseMode.ReleaseFull

      case ReleaseSize => model.NativeLinkerReleaseMode.ReleaseSize

    }

    override def from(t2: model.NativeLinkerReleaseMode): Config.NativeLinkerReleaseMode = t2 match {
      case model.NativeLinkerReleaseMode.ReleaseFast => Config.NativeLinkerReleaseMode.ReleaseFast
      case model.NativeLinkerReleaseMode.ReleaseFull => Config.NativeLinkerReleaseMode.ReleaseFull
      case model.NativeLinkerReleaseMode.ReleaseSize => Config.NativeLinkerReleaseMode.ReleaseSize
    }

  }

  object nativeLTO extends Bijection[Config.NativeLTO, model.NativeLTO] {

    override def to(t1: Config.NativeLTO): model.NativeLTO = t1 match {
      case Full                  => model.NativeLTO.Full
      case Config.NativeLTO.None => model.NativeLTO.None
      case Thin                  => model.NativeLTO.Thin
    }

    override def from(t2: model.NativeLTO): Config.NativeLTO = t2 match {
      case model.NativeLTO.Full => Config.NativeLTO.Full
      case model.NativeLTO.None => Config.NativeLTO.None
      case model.NativeLTO.Thin => Config.NativeLTO.Thin
    }

  }
}
