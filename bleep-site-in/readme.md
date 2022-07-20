---
sidebar_position: 1
---

# About Bleep

Yes, yes, yes. Yet another build tool.

Bleep emphasizes
- **fast**
- **simple**
- **automatic import from sbt**
- **one second project import into your IDE through BSP**
- **top-notch CLI experience**

Intrigued? Keep reading!

## Status

Bleep automatically imports and builds huge projects, like tapir or http4s.

It's early days, but most major things are already working. This is a great time to test it and get involved! 

## What, exactly?

If you have an interest in build tools you'll probably know Bloop, the Scala build server.

Bloop has already defined an almost perfect json format for describing how to build, run, test, debug, link and so on.
This json format has two drawbacks:
- bound to one machine
- verbose

Bleep uses coursier to enable you to write essentially portable Bloop json files without the boilerplate.

Bleep is compiled with native image to be fast, it starts ~instantly.

<details>
  <summary>Example file: bleep.yaml used to compile bleep</summary>

```yaml
@BLEEPYAML@
```

</details>

## Demo videos 
### IDE import
<video
controls="true"
src="https://user-images.githubusercontent.com/247937/144330278-9eac4179-71fb-4937-8ac5-3c94df76caa0.mp4">
</video>

### CLI

Bleep also aims to provide a top-notch CLI experience

<video
controls="true"
src="https://user-images.githubusercontent.com/247937/144334308-215c1351-11d3-4452-ad66-2529fd7450f1.mp4">
</video>





## How to build bleep

You should have following on your `$PATH`:

* Java JDK 11
* `sbt`
* `native-image`

then
```
# builds image, very slow
sbt> nativeImage

# runs image, very fast 
sbt> nativeImageRun  
```

## Acknowledgements

All the hard work is already done by
[Bloop](https://github.com/scalacenter/bloop),
[Coursier](https://github.com/coursier/coursier),
and [GraalVM native image](https://www.graalvm.org/reference-manual/native-image/).

The basis for the model used by bleep is defined by Bloop.

Bleep integrates code from some external projects:

- the code for integrating with BSP is copied and/or heavily inspired by [scala-cli](https://github.com/VirtusLab/scala-cli)
- a few sbt plugins. the exact list can be found in [.gitmodules](https://github.com/oyvindberg/bleep/blob/master/.gitmodules).
