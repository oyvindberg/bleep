# Title

`scala.js: bleep test runs no munit suite and no utest suite on 1.0.0-M12`

# Body

**TL;DR.** `bleep test` runs no Scala.js suite on 1.0.0-M12 because the injected harness
drives the linked output through mangled Scala.js internal names. A munit link declares
none of the names the harness expects, and every munit suite fails at load. A utest suite
loads and then dies inside `TestRunner.runAsync` after a 30 second wait. Routing Scala.js
tests through the standard `TestAdapter` and `Bridge` repairs both and works for every
sbt-testing framework.

---

`bleep test` on a Scala.js project compiles the project, links it, and discovers the test
suites correctly. Every suite then fails inside the JavaScript harness that
`bleep.bsp.ScalaJsTestRunner` injects. A munit suite fails at load with `Could not load
test module via Reflect`. A utest suite fails after a 30 second wait inside
`utest.TestRunner.runAsync`.

The harness drives the linked output by looking up Scala.js internal symbols under their
mangled JavaScript names. Which mangled names a linked program declares depends on which
code that program reaches, and the names the harness expects are not the names munit
produces.

## Reproducing this takes three projects and three files

`bleep.yaml`

```yaml
$schema: https://raw.githubusercontent.com/oyvindberg/bleep/master/schema.json
$version: 1.0.0-M12
jvm:
  name: corretto:25.0.4.8.1
projects:
  jstest:
    dependencies:
    - org.scalameta::munit:1.3.5
    isTestProject: true
    platform:
      name: js
      jsVersion: 1.22.0
      jsNodeVersion: 24.18.0
      jsKind: none
    scala:
      version: 3.8.4
  jvmtest:
    dependencies:
    - org.scalameta::munit:1.3.5
    isTestProject: true
    platform:
      name: jvm
    scala:
      version: 3.8.4
  utesttest:
    dependencies:
    - com.lihaoyi::utest:0.9.1
    isTestProject: true
    platform:
      name: js
      jsVersion: 1.22.0
      jsNodeVersion: 24.18.0
      jsKind: none
    scala:
      version: 3.8.4
```

`jstest/src/scala/repro/ClassSpec.scala`, copied byte for byte to
`jvmtest/src/scala/repro/ClassSpec.scala`

```scala
package repro

class ClassSpec extends munit.FunSuite:

  test("one plus one is two") {
    assertEquals(1 + 1, 2)
  }
```

`utesttest/src/scala/repro/UtestSpec.scala`

```scala
package repro

import utest._

object UtestSpec extends TestSuite:
  val tests = Tests {
    test("two plus two is four") {
      assert(2 + 2 == 4)
    }
  }
```

`bleep test jvmtest` passes.

```
[info ]   Tests: 1 passed, 0 failed
[info ]   Suites: 1 total
```

`bleep test jstest` fails on the identical source.

```
[info ] 🔍 discovered 1 test suites (total: 1) [project => jstest]
[info ] FAILED repro.ClassSpec: 0 passed, 1 failed, 0 skipped (511 ms)

[info ] ! jstest / repro.ClassSpec
[info ]   | Suite reported 1 failure(s) but no individual test results were captured
[info ]   Output:
[info ]   | Could not load test module via Reflect: repro.ClassSpec$
```

`bleep test utesttest` fails after 30 seconds.

```
[info ] 🔍 discovered 1 test suites (total: 1) [project => utesttest]
[info ] NO TESTS repro.UtestSpec: discovered but executed 0 tests (30412 ms)

[info ] ! utesttest / repro.UtestSpec
[info ]   | Suite repro.UtestSpec was discovered but executed 0 tests
[info ]   Output:
[info ]   | scala.scalajs.js.JavaScriptException: TypeError: $n(...).apply__O__O is not a function
[info ]   |   at scala.concurrent.impl.Promise$Transformation.run(.../main.js:54210:17)
```

I expected the Scala.js runs to pass, matching the JVM run and matching what sbt and mill
produce through the standard Scala.js test adapter.

## A munit suite fails because the harness looks up a Reflect module the linker did not emit

`bleep-bsp/src/scala/bleep/bsp/ScalaJsTestRunner.scala:411` looks the reflection entry
point up under one fixed mangled name.

```js
const ReflectGetter = sandbox[dollar + 'm_Lorg_portablescala_reflect_Reflect' + dollar];
```

The Scala.js linker declares a module accessor only for a module the linked program
reaches. utest's framework code calls `org.portablescala.reflect.Reflect`. munit calls
`scala.scalajs.reflect.Reflect` from `munit.internal.PlatformCompat`. Counting the two
names with `grep -c` on the two linked `main.js` files this reproduction produced gives:

| linked output | `$m_Lorg_portablescala_reflect_Reflect$` | `$m_sjs_reflect_Reflect$` |
| --- | --- | --- |
| `utesttest` | 3 | 9 |
| `jstest` | 0 | 10 |

`ReflectGetter` is `undefined` for the munit project. `Reflect` becomes `null`. The
loader at line 436 then reports `Could not load test module via Reflect` for every suite
in the project, whatever the suite is. Replacing the class in the reproduction with a
Scala `object` changes nothing, even though the linked output registers that object
through `registerLoadableModuleClass("repro.ObjectSpec$", ...)` and invokes the
registration before the bridge starts.

## The loader needs a class branch for munit

`ScalaJsTestRunner.scala:422` appends a dollar sign to the suite name. Line 426 calls
`lookupLoadableModuleClass` and nothing else. munit's `Framework` declares
`SubclassFingerprint(superclassName = "munit.Suite", isModule = false,
requireNoArgConstructor = true)`, which makes a munit suite a class rather than an
object. The JVM run above confirms that reading. munit's own `PlatformCompat.newRunner`
calls `Reflect.lookupInstantiatableClass(taskDef.fullyQualifiedName())` with no dollar
sign, which line 8001 of the linked `main.js` for `jstest` shows. A working Reflect
lookup alone would still leave `class X extends munit.FunSuite` unable to load, because
the harness declares no `lookupInstantiatableClass` plus `newInstance` path.

## The runner drives utest only

`ScalaJsTestRunner.scala:443` fetches `tests__Lutest_Tests` from the loaded suite. Line
445 reports `No tests__Lutest_Tests method found on suite` when that method is absent.
`runTests` declares no munit execution path. The discovery script's `isTestSuite` already
detects `prototype.munitTests`, and the runner has no branch matching that detection. A
munit suite that loaded successfully would fail here instead.

## The utest suite fails as well

The `TypeError` above comes from inside `utest.TestRunner.runAsync`, which the harness
calls at `ScalaJsTestRunner.scala:458` onward with hand-built `AnonFunction1` and
`AnonFunction2` instances. I did not root-cause that failure. The 30 second wait before
the run gives up comes from the hard-coded `setTimeout(() => resolve(), 30000)` at line
533.

## The linked output already declares the standard bridge

Every linked test `main.js` this reproduction produced ends on the standard entry point.

```js
$sct_Lmunit_Framework__stinit__();
$sct_Lrepro_ClassSpec__stinit__();
$s_Lorg_scalajs_testing_bridge_Bridge__start__V();
```

The harness deletes that call with a regular expression and takes the run over.

## Proposed change

Drive Scala.js tests through `org.scalajs.testing.adapter.TestAdapter` on the JVM side
and `org.scalajs.testing.bridge.Bridge` on the JavaScript side, which is the path sbt and
mill take. `TestAdapter` speaks the `sbt.testing` interfaces that bleep's JVM-side
discovery already uses. It works for munit, scalatest, specs2, and utest without a code
path per framework, and it removes every dependence on a mangled name that the linker
declares only when the program happens to reach that code. `TestAdapter` also accepts a
`JSEnv`, which gives a later hook for `JSDOMNodeJSEnv` when a suite touches the DOM.

Two cheaper repairs would fix munit alone. The loader could fall back to
`lookupInstantiatableClass(suiteName)` plus `newInstance`, and `runTests` could grow a
munit branch. Both keep the mangled-name coupling that produced this defect.

You said in discussion #654 that a pull request for the `TestAdapter` route is welcome. I
am ready to write it against this issue.

## Relation to #233

[#233](https://github.com/oyvindberg/bleep/issues/233) reports Scala.js tests hanging on
0.0.1-M22 and Scala Native frameworks going undetected. The harness described here
replaced the code that #233 reported on, and the version, the symptoms, and the root
cause are all different. Close #233 as covered by this issue if you prefer to track one.

## Environment

- bleep 1.0.0-M12, with the harness source unchanged on master at `b0cea39f`
- macOS 26.5.2 on arm64
- JVM `corretto:25.0.4.8.1`, managed by bleep
- Scala 3.8.4, Scala.js 1.22.0, node 24.18.0 managed by bleep
- munit 1.3.5, utest 0.9.1
