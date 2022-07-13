---
sidebar_position: 2
---


# But my build does much more!

Yes, your build does other things.

It generates code, it distributes artifacts, it builds websites. Probably a lot more

But none of this has to be in the build itself!

### Scripts
Bleep introduces `scripts`.
Defined in your build file as the `scripts` in a `package.json`, it allows you to run any class with a `main` method.
```json
{
  "scripts": {
    "native-image": "mymodule/mypackage.GenNativeImage"
  }
}
```
   
### Running
You can run the script like this, it can be tab-completed to save some keystrokes
```sh
$ bleep native-image
```

### An example

Bleep provides you with the entire build in a structured format, so you can query projects,
paths and so on.

```scala
import bleep.model
import bleep.tasks._

object GenNativeImage {
  def main(args: Array[String]): Unit =
    bleep.bootstrap.forScript("GenNativeImage") { (started, commands) =>
      
      // the project we want to build native image of
      val projectName = model.CrossProjectName(model.ProjectName("bleep-cli"), crossId = None)
      // lookup the bloop project
      val project = started.bloopProjects(projectName)
      // ensure we have built it
      commands.compile(List(projectName))
      
      // setup native-image plugin
      val plugin = new NativeImagePlugin(
        project = project,
        logger = started.logger,
        nativeImageOptions = List("--no-fallback", "-H:+ReportExceptionStackTraces"),
        nativeImageJvm = started.rawBuild.jvm.getOrElse(model.Jvm.graalvm)
      ) 
      // run
      val path = plugin.nativeImage()
      started.logger.info(s"Created native-image at $path")
    }
}

```

It might already be clear to you, but anyways:

- you can start this program from bleep (`bleep native-image`), from your IDE, from anywhere
- you can debug it like a normal program
- you can include any dependencies you want, from any scala version. No class loader issues.
- you have all the structured info from the build, without having to express code in your build
- the NativeImagePlugin is directly ported from sbt. In fact most interesting sbt plugins can be ported quite easily! setting = `val`, task = `def`.
