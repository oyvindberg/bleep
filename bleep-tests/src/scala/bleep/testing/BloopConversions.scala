package bleep.testing

import bleep.{model, ResolvedProject}
import bloop.config.Config

import java.nio.file.Paths

/** Conversions between bleep's ResolvedProject and bloop's Config types.
  *
  * This is only used in tests for comparing bloop file output.
  */
object BloopConversions {

  /** Convert a ResolvedProject to bloop Config.File for test comparisons */
  def toBloopConfig(resolved: ResolvedProject): Config.File = {
    val scalaConfig = resolved.language match {
      case s: ResolvedProject.Language.Scala =>
        Some(
          Config.Scala(
            organization = s.organization,
            name = s.name,
            version = s.version,
            options = s.options,
            jars = s.compilerJars,
            analysis = s.analysisFile,
            setup = s.setup.map(setup =>
              Config.CompileSetup(
                order = compileOrderFrom(setup.order),
                addLibraryToBootClasspath = setup.addLibraryToBootClasspath,
                addCompilerToClasspath = setup.addCompilerToClasspath,
                addExtraJarsToClasspath = setup.addExtraJarsToClasspath,
                manageBootClasspath = setup.manageBootClasspath,
                filterLibraryFromClasspath = setup.filterLibraryFromClasspath
              )
            ),
            bridgeJars = None
          )
        )
      case _ => None
    }

    val javaConfig = Some(Config.Java(options = resolved.language.javaOptions))

    val platformConfig = resolved.platform.map {
      case ResolvedProject.Platform.Jvm(options, mainClass, runtimeOptions) =>
        Config.Platform.Jvm(
          config = Config.JvmConfig(home = None, options = options),
          mainClass = mainClass,
          runtimeConfig = if (runtimeOptions.nonEmpty) Some(Config.JvmConfig(home = None, options = runtimeOptions)) else None,
          classpath = None,
          resources = None
        )
      case ResolvedProject.Platform.Js(version, mode, kind, emitSourceMaps, jsdom, nodePath, mainClass) =>
        Config.Platform.Js(
          config = Config.JsConfig(
            version = version,
            mode = if (mode == "release") Config.LinkerMode.Release else Config.LinkerMode.Debug,
            kind = kind match {
              case "CommonJSModule"        => Config.ModuleKindJS.CommonJSModule
              case "ESModule"              => Config.ModuleKindJS.ESModule
              case "NoModule" | "nomodule" => Config.ModuleKindJS.NoModule
              case _                       => Config.ModuleKindJS.NoModule
            },
            emitSourceMaps = emitSourceMaps,
            jsdom = jsdom,
            output = None,
            nodePath = nodePath,
            toolchain = Nil,
            moduleSplitStyle = None
          ),
          mainClass = mainClass
        )
      case ResolvedProject.Platform.Native(version, mode, gc, mainClass) =>
        Config.Platform.Native(
          config = Config.NativeConfig(
            version = version,
            mode = if (mode == "release") Config.LinkerMode.Release else Config.LinkerMode.Debug,
            gc = gc,
            targetTriple = None,
            clang = Paths.get("clang"),
            clangpp = Paths.get("clang++"),
            toolchain = Nil,
            options = Config.NativeOptions(Nil, Nil),
            linkStubs = false,
            check = false,
            dump = false,
            output = None,
            buildTarget = None,
            nativeModeAndLTO = Config.NativeModeAndLTO.empty,
            nativeFlags = Config.NativeFlags.empty
          ),
          mainClass = mainClass
        )
    }

    val resolutionConfig = resolved.resolution.map { r =>
      Config.Resolution(
        r.modules.map { m =>
          Config.Module(
            organization = m.organization,
            name = m.name,
            version = m.version,
            configurations = None,
            artifacts = m.artifacts.map { a =>
              Config.Artifact(
                name = a.name,
                classifier = a.classifier,
                checksum = None,
                path = a.path
              )
            }
          )
        }
      )
    }

    val testConfig = if (resolved.isTestProject) {
      val frameworks =
        if (resolved.testFrameworks.nonEmpty)
          List(Config.TestFramework(resolved.testFrameworks))
        else
          Config.Test.defaultConfiguration.frameworks
      Some(Config.Test(frameworks, Config.Test.defaultConfiguration.options))
    } else None

    Config.File(
      version = "1.4.0",
      project = Config.Project(
        name = resolved.name,
        directory = resolved.directory,
        workspaceDir = Some(resolved.workspaceDir),
        sources = resolved.sources,
        sourcesGlobs = None,
        sourceRoots = None,
        dependencies = resolved.dependencies,
        classpath = resolved.classpath,
        out = resolved.directory,
        classesDir = resolved.classesDir,
        resources = resolved.resources,
        scala = scalaConfig,
        java = javaConfig,
        sbt = None,
        test = testConfig,
        platform = platformConfig,
        resolution = resolutionConfig,
        tags = None,
        sourceGenerators = None
      )
    )
  }

  private def compileOrderFrom(order: model.CompileOrder): Config.CompileOrder = order match {
    case model.CompileOrder.JavaThenScala => Config.JavaThenScala
    case model.CompileOrder.ScalaThenJava => Config.ScalaThenJava
  }
}
