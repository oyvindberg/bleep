# A bleeping fast scala build tool!

[![Join the chat at https://gitter.im/oyvindberg/bleep](https://badges.gitter.im/oyvindberg/bleep.svg)](https://gitter.im/oyvindberg/bleep?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

See documentation at [https://bleep.build](https://bleep.build/docs/)


### Contributing

#### Check out bleep source code
```
$ git clone --recurse-submodules https://github.com/oyvindberg/bleep.git
```

#### Install bleep 
See https://bleep.build/docs/installing/

#### Use bleep to build bleep
```
$ bleep compile jvm213
```

#### Setup in IDE

```
$ bleep setup-ide jvm213
```
And then open in using metals or intellij.

See [setting-up-build-in-ide](https://bleep.build/docs/usage/selecting-projects/#setting-up-build-in-ide) for a bit more info.

#### Run bleep

You have several options:
- run with bleep `bleep run bleep-cli@jvm213`
- run `bleep.Main` from your IDE
- generate a native image (`bleep native-image`) and run that (the path will be printed).
This is slow, but a few things can only be tested this way.
- generate a shell wrapper script:
```sh
# generate script in build directory
$ bleep setup-dev-script bleep-cli@jvm213

# with this approach you need to compile manually
$ bleep compile bleep-cli@jvm213

$ ./bleep-cli@jvm213.sh
```
Generating shell wrapper scripts is currently not implemented on windows, but it's likely easy to add if you want it.

#### Submitting code
- All changes go through PRs
- Code should be formatted correctly (`bleep fmt`) and tests should pass (`bleep test`).
