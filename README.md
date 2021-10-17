# bleep

Named after the censored expletive a developer will mutter almost inaudibly after
seeing how damn fast it starts compiling your scala code.

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