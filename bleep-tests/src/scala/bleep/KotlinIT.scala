package bleep

class KotlinIT extends IntegrationTestHarness {
  integrationTest("kotlin compilation") { ws =>
    ws.yaml(
      """projects:
        |  myapp:
        |    source-layout: kotlin
        |    kotlin:
        |      version: 2.1.20
        |    platform:
        |      name: jvm
        |""".stripMargin
    )
    ws.file(
      "myapp/src/main/kotlin/test/Hello.kt",
      """package test
        |
        |fun greet(name: String): String = "Hello, $name!"
        |""".stripMargin
    )
    val (_, commands, _) = ws.start()
    commands.compile(List(model.CrossProjectName(model.ProjectName("myapp"), None)))
    succeed
  }

  integrationTest("kotlin test with JUnit") { ws =>
    ws.yaml(
      """projects:
        |  myapp:
        |    source-layout: kotlin
        |    kotlin:
        |      version: 2.1.20
        |    platform:
        |      name: jvm
        |  myapp-test:
        |    dependencies:
        |    - com.github.sbt:junit-interface:0.13.3
        |    - org.jetbrains.kotlin:kotlin-test-junit:2.1.20
        |    dependsOn: myapp
        |    isTestProject: true
        |    source-layout: kotlin
        |    kotlin:
        |      version: 2.1.20
        |    platform:
        |      name: jvm
        |    testFrameworks: com.novocode.junit.JUnitFramework
        |""".stripMargin
    )
    ws.file(
      "myapp/src/main/kotlin/test/Hello.kt",
      """package test
        |
        |fun greet(name: String): String = "Hello, $name!"
        |""".stripMargin
    )
    ws.file(
      "myapp-test/src/test/kotlin/test/HelloTest.kt",
      """package test
        |
        |import org.junit.Test
        |import kotlin.test.assertEquals
        |
        |class HelloTest {
        |    @Test
        |    fun testGreet() {
        |        assertEquals("Hello, World!", greet("World"))
        |    }
        |}
        |""".stripMargin
    )
    val (_, commands, _) = ws.start()
    commands.test(
      projects = List(model.CrossProjectName(model.ProjectName("myapp-test"), None)),
      watch = false,
      only = None,
      exclude = None
    )
    succeed
  }

  integrationTest("kotlin internal visibility with friend-paths") { ws =>
    ws.yaml(
      """projects:
        |  mylib:
        |    source-layout: kotlin
        |    kotlin:
        |      version: 2.1.20
        |    platform:
        |      name: jvm
        |  mylib-test:
        |    dependencies:
        |    - com.github.sbt:junit-interface:0.13.3
        |    - org.jetbrains.kotlin:kotlin-test-junit:2.1.20
        |    dependsOn: mylib
        |    isTestProject: true
        |    source-layout: kotlin
        |    kotlin:
        |      version: 2.1.20
        |    platform:
        |      name: jvm
        |    testFrameworks: com.novocode.junit.JUnitFramework
        |""".stripMargin
    )
    ws.file(
      "mylib/src/main/kotlin/mylib/Internal.kt",
      """package mylib
        |
        |internal fun secretGreet(name: String): String = "Secret hello, $name!"
        |""".stripMargin
    )
    ws.file(
      "mylib-test/src/test/kotlin/mylib/InternalTest.kt",
      """package mylib
        |
        |import org.junit.Test
        |import kotlin.test.assertEquals
        |
        |class InternalTest {
        |    @Test
        |    fun testInternalAccess() {
        |        assertEquals("Secret hello, World!", secretGreet("World"))
        |    }
        |}
        |""".stripMargin
    )
    val (_, commands, _) = ws.start()
    commands.test(
      projects = List(model.CrossProjectName(model.ProjectName("mylib-test"), None)),
      watch = false,
      only = None,
      exclude = None
    )
    succeed
  }

  integrationTest("kotlin compiler plugins (allopen)") { ws =>
    ws.yaml(
      """projects:
        |  myapp:
        |    source-layout: kotlin
        |    kotlin:
        |      version: 2.1.20
        |      compilerPlugins:
        |      - allopen
        |      options: -P plugin:org.jetbrains.kotlin.allopen:annotation=myapp.MyOpen
        |    platform:
        |      name: jvm
        |  myapp-test:
        |    dependencies:
        |    - com.github.sbt:junit-interface:0.13.3
        |    - org.jetbrains.kotlin:kotlin-test-junit:2.1.20
        |    dependsOn: myapp
        |    isTestProject: true
        |    source-layout: kotlin
        |    kotlin:
        |      version: 2.1.20
        |    platform:
        |      name: jvm
        |    testFrameworks: com.novocode.junit.JUnitFramework
        |""".stripMargin
    )
    ws.file(
      "myapp/src/main/kotlin/myapp/MyOpen.kt",
      """package myapp
        |
        |annotation class MyOpen
        |""".stripMargin
    )
    ws.file(
      "myapp/src/main/kotlin/myapp/Service.kt",
      """package myapp
        |
        |// The allopen plugin should make this class open (non-final)
        |@MyOpen
        |class MyService {
        |    fun greet(name: String): String = "Hello, $name!"
        |}
        |""".stripMargin
    )
    ws.file(
      "myapp-test/src/test/kotlin/myapp/ServiceTest.kt",
      """package myapp
        |
        |import org.junit.Test
        |import kotlin.test.assertEquals
        |
        |// This subclass would fail to compile without allopen plugin
        |// because Kotlin classes are final by default
        |class TestService : MyService() {
        |    @Test
        |    fun testGreet() {
        |        assertEquals("Hello, World!", greet("World"))
        |    }
        |}
        |""".stripMargin
    )
    val (_, commands, _) = ws.start()
    commands.test(
      projects = List(model.CrossProjectName(model.ProjectName("myapp-test"), None)),
      watch = false,
      only = None,
      exclude = None
    )
    succeed
  }
}
