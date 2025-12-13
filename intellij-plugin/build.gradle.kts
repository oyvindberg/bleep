import org.jetbrains.kotlin.gradle.dsl.JvmTarget

plugins {
    id("java")
    id("org.jetbrains.kotlin.jvm") version "2.1.0"
    id("org.jetbrains.intellij.platform") version "2.10.5"
}

group = providers.gradleProperty("pluginGroup").get()
version = providers.gradleProperty("pluginVersion").get()

repositories {
    mavenCentral()
    intellijPlatform {
        defaultRepositories()
    }
}

dependencies {
    intellijPlatform {
        intellijIdea(providers.gradleProperty("platformVersion"))
        bundledPlugins(providers.gradleProperty("platformBundledPlugins").map { it.split(',').map(String::trim).filter { it.isNotEmpty() } })
        plugins(providers.gradleProperty("platformPlugins").map { it.split(',').map(String::trim).filter { it.isNotEmpty() } })
        pluginVerifier()
    }

    // YAML parsing
    implementation("org.yaml:snakeyaml:2.3")

    // JSON parsing for bleep extract-info output
    implementation("com.google.code.gson:gson:2.11.0")

    // For archive extraction
    implementation("org.apache.commons:commons-compress:1.27.1")

    // Note: Can optionally add bleep-model for fuller integration
    // implementation("build.bleep:bleep-model_3:0.0.12")

    testImplementation("junit:junit:4.13.2")
}

kotlin {
    jvmToolchain(providers.gradleProperty("javaVersion").get().toInt())
}

intellijPlatform {
    pluginConfiguration {
        name = providers.gradleProperty("pluginName")
        version = providers.gradleProperty("pluginVersion")

        ideaVersion {
            sinceBuild = "253"
            untilBuild = provider { null }
        }
    }

    signing {
        certificateChain = providers.environmentVariable("CERTIFICATE_CHAIN")
        privateKey = providers.environmentVariable("PRIVATE_KEY")
        password = providers.environmentVariable("PRIVATE_KEY_PASSWORD")
    }

    publishing {
        token = providers.environmentVariable("PUBLISH_TOKEN")
    }

    pluginVerification {
        ides {
            recommended()
        }
    }
}

tasks {
    wrapper {
        gradleVersion = "8.13"
    }

    compileKotlin {
        compilerOptions {
            jvmTarget.set(JvmTarget.JVM_21)
        }
    }

    compileJava {
        sourceCompatibility = "21"
        targetCompatibility = "21"
    }
}
