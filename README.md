# A bleeping fast scala build tool!

Yes, yes, yes. Yet another build tool. 

Bleep emphasizes 
- **fast**
- **simple**
- **automatic import from sbt**
- **one second project import into your IDE through BSP**

Intrigued? Keep reading!


https://user-images.githubusercontent.com/247937/144330278-9eac4179-71fb-4937-8ac5-3c94df76caa0.mp4


## Status

Bleep automatically imports and builds huge projects, like tapir or http4s. 

However, it's very early days. There hasn't been a milestone release yet. 

So please try it out, please get involved. But let's not spread the word too widely just yet. 
Even the name is likely to change.

## What, exactly?

If you have an interest in build tools you'll probably know Bloop, the Scala build server.

Bloop has already defined an almost perfect json format for describing how to build, run, test, debug, link and so on.
This json format has two drawbacks:
- bound to one machine
- verbose

Bleep uses coursier to enable you to write essentially portable Bloop json files without the boilerplate.

Bleep is compiled with native image to be fast, it starts ~instantly.

For an up to date example of what a bleep build file see [bleep.yaml in this repo](./bleep.yaml).

## But what about X?

Yes, your build does other things. 

It generates code, it distributes artifacts, it builds websites. Probably a lot more

But none of this has to be in the build itself!

Bleep introduces `scripts`. 
Defined in your build file as the `scripts` in a `package.json`, it allows you to run any class with a `main` method.
```json
{
  "scripts": {
    "native-image": "mymodule/mypackage.GenNativeImage"
  }
}
```

Bleep provides you with the entire build in a structured format, so you can query projects,
paths and so on.

```scala
import bleep._
import bleep.tasks._

object GenNativeImage extends BleepScript("GenNativeImage") {
  def run(started: Started, commands: Commands, args: List[String]): Unit = {
    val projectName = model.ProjectName("myproject")
    val project = started.bloopFiles(projectName).forceGet

    val plugin = new NativeImagePlugin(project.project, started.logger, nativeImageOptions = List("--no-fallback", "-H:+ReportExceptionStackTraces"))
    val path = plugin.nativeImage()
    started.logger.info(s"Created native-image at $path")
  }
}
```

It might already be clear to you, but anyways:

- you can start this program from Bleep (`bleep native-image`), from your IDE, from anywhere
- you can debug it like a normal program
- you can include any dependencies you want, from any scala version. No class loader issues.
- you have all the structured info from the build, without having to express code in your build
- the NativeImagePlugin is directly ported from sbt. In fact most interesting sbt plugins can be ported quite easily! setting = `val`, task = `def`.

## CLI

Bleep also aims to provide a top notch CLI experience


https://user-images.githubusercontent.com/247937/144334308-215c1351-11d3-4452-ad66-2529fd7450f1.mp4

## How to install Bleep

1) install [Coursier CLI](https://get-coursier.io)
2) `cs install --channel https://raw.githubusercontent.com/oyvindberg/bleep/master/coursier-channel.json bleep`

## How to build Bleep

Bleep is built with Bleep, so you'll need to install it first

1) clone repository
2) `git submodule init`
3) `git submodule update`
4) `bleep native-image`

Step 4) is rather slow, and for most purposes you can use the `bleep_dev.sh` script instead.

## Acknowledgements

All the hard work is already done by 
[Bloop](https://github.com/scalacenter/bloop),
[Coursier](https://github.com/coursier/coursier),
and [GraalVM native image](https://www.graalvm.org/reference-manual/native-image/).

The basis for the model used by bleep is defined by Bloop.

Bleep integrates code from some external projects:

- the code for integrating with BSP is copied and/or heavily inspired by [scala-cli](https://github.com/VirtusLab/scala-cli)
- a few sbt plugins. the exact list can be found in [.gitmodules](./.gitmodules).
