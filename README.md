# A bleeping fast scala build tool!

[![Scala Discord](https://dcbadge.limes.pink/api/server/https://discord.gg/scala)](https://discord.gg/scala) Chat in the [#tooling channel](https://discord.com/channels/632150470000902164/635669047588945930) of the Scala Discord

See documentation at [https://bleep.build](https://bleep.build/docs/)


### Contributing

#### Check out bleep source code
```
$ git clone --recurse-submodules https://github.com/oyvindberg/bleep.git
```

#### Install bleep 
See https://bleep.build/docs/installing/

#### Use bleep to build bleep
```bash
# compile all projects using scala 2.13
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
