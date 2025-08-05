# Bleep Build Tool - Developer Guide

This document provides comprehensive background for working with bleep, a modern Scala build tool that emphasizes speed, simplicity, and excellent developer experience.

## Overview

Bleep is a native-image compiled build tool for Scala that replaces sbt with:
- **Instant startup** (~0ms) through GraalVM native image compilation
- **Declarative YAML configuration** instead of Scala DSL
- **Excellent IDE integration** through BSP (1-second imports)
- **Simple script system** replacing sbt's complex task/plugin model
- **Fast compilation** via Bloop build server

## Core Concepts

### 1. Build Configuration Structure

Bleep uses YAML for build configuration (`bleep.yaml`):

```yaml
$schema: https://raw.githubusercontent.com/oyvindberg/bleep/master/schema.json
$version: 0.0.12
jvm:
  name: graalvm-community:24.0.1
projects:
  myapp:
    dependencies: 
      - com.lihaoyi::fansi:0.3.1  # Scala dependency (double colon)
      - org.scalameta:svm-subs:101.0.0  # Java dependency (single colon)
    dependsOn: myapp-core
    extends: template-common
    platform:
      mainClass: com.example.Main
templates:
  template-common:
    scala:
      version: 3.3.3
scripts:
  generate-docs:
    main: bleep.scripts.GenDocumentation
    project: scripts
```

### 2. Templates System

Templates reduce duplication through inheritance:
- Projects extend templates with `extends: template-name`
- Templates can extend other templates
- Common patterns: `template-common`, `template-cross-all`, `template-scala-3`

### 3. Cross-Building

Cross-building for different Scala versions/platforms:

```yaml
projects:
  mylib:
    cross:
      jvm213:
        scala:
          version: 2.13.11
      jvm3:
        scala:
          version: 3.3.3
      js3:
        platform:
          name: js
          jsVersion: 1.13.2
```

### 4. Java Annotation Processors

Bleep supports Java annotation processors with explicit configuration:

```yaml
projects:
  myapp:
    dependencies:
      - org.projectlombok:lombok:1.18.30
    java:
      annotationProcessors:
        - className: lombok.launch.AnnotationProcessorHider$AnnotationProcessor
        - className: com.google.auto.value.processor.AutoValueProcessor
```

Key points:
- Annotation processors must be explicitly listed by their fully qualified class names
- Processors must be available on the runtime classpath (via dependencies)
- When processors are configured, bleep automatically:
  - Creates a generated sources directory at `.bleep/generated-sources/<build>/<project>/annotations`
  - Passes `-processor` option to javac with comma-separated list of processors
  - Adds `-s` option pointing to the generated sources directory
- Without any processors configured, bleep adds `-proc:none` to disable annotation processing

### 5. Scripts (Replacing sbt Tasks/Plugins)

Scripts are regular Scala programs that replace sbt's task system:

```scala
object MyScript extends BleepScript("MyScript") {
  override def run(started: Started, commands: Commands, args: List[String]): Unit = {
    // Access build information
    val build = started.build.exploded
    // Run commands
    commands.compile(started.build.explodedProjects.keys.toList)
  }
}
```

Key advantages:
- Can be debugged like normal programs
- No classloader issues
- Full access to build model
- Can use any dependencies

## Working with Liberated Projects

The `liberated/` directory contains sbt projects ported to bleep. Each is a git submodule with:
- `origin` remote pointing to bleep-build fork
- `origin-original` remote pointing to upstream project

### Porting Patterns from sbt to bleep

Based on analysis of sbt-ci-release and sbt-sonatype ports:

1. **Package Restructuring**
   - Move to `bleep.plugin.*` namespace
   - Example: `com.geirsson.CiReleasePlugin` → `bleep.plugin.cirelease.CiReleasePlugin`

2. **Dependency Changes**
   - Replace sbt-specific dependencies with standard libraries
   - Use bleep's logging (ryddig) instead of sbt's logger
   - Example: sbt's `Keys` → direct parameter passing

3. **Plugin Architecture**
   - sbt plugins become regular classes with constructor parameters
   - No more `AutoPlugin` trait or `override def requires`
   - Settings become constructor parameters or method arguments

4. **Build Definition**
   - `build.sbt` → included directly in parent's `bleep.yaml`
   - Source directories referenced via `sources:` field
   - Example:
     ```yaml
     bleep-plugin-ci-release:
       sources: ../liberated/sbt-ci-release/plugin/src/main/scala
       dependsOn:
         - bleep-plugin-dynver
         - bleep-plugin-pgp
         - bleep-plugin-sonatype
     ```

5. **API Adaptations**
   - Replace sbt tasks with method calls
   - Use bleep's model types (e.g., `model.Repository`)
   - Logging: `sbt.Logger` → `ryddig.Logger`

6. **Cross-Compilation**
   - Use `for3Use213: true` for libraries not yet on Scala 3
   - Handle binary compatibility carefully

## Bleep's Own Build Structure

Bleep uses itself to build, demonstrating key patterns:

1. **Core Projects**
   - `bleep-model`: Data types for build representation
   - `bleep-core`: Build logic and compilation
   - `bleep-cli`: Command-line interface
   - `bleep-nosbt`: Liberated sbt librarymanagement code

2. **Plugin Projects** (all from liberated/)
   - `bleep-plugin-ci-release`: CI/CD automation
   - `bleep-plugin-sonatype`: Maven Central publishing
   - `bleep-plugin-dynver`: Dynamic versioning
   - `bleep-plugin-pgp`: Artifact signing
   - `bleep-plugin-native-image`: GraalVM native image generation
   - `bleep-plugin-mdoc`: Documentation generation
   - `bleep-plugin-scalafix`: Code linting/rewriting

3. **Scripts**
   - `publish`: Release to Maven Central
   - `native-image`: Build bleep executable
   - `generate-docs`: Build documentation

## Key Commands

```bash
# Basic operations
bleep compile [projects]
bleep test [projects]
bleep run [project]

# Publishing
bleep publish-local [projects]
bleep scripts publish  # Custom publish script

# Project management
bleep projects  # List all projects
bleep build update-deps  # Update dependencies

# Import from sbt
bleep import  # Convert sbt build to bleep
```

## Development Workflow

1. **Compiling Bleep Itself**
   ```bash
   bleep compile  # Compiles all projects
   ```

2. **Running Tests**
   ```bash
   bleep test bleep-tests
   ```

3. **Building Native Image**
   ```bash
   bleep scripts native-image
   ```

4. **Working with Liberated Projects**
   - Make changes in `liberated/project-name/`
   - Test with bleep commands
   - Changes automatically picked up by parent build

## Important Design Decisions

1. **Declarative Configuration**: Build files are data, not code
2. **Speed First**: Native compilation, Bloop server, efficient algorithms
3. **Simplicity**: No scopes, no complex DSL, scripts are just programs
4. **Portability**: All paths relative, dependencies via Maven coordinates
5. **Reproducibility**: Managed JVM/Node versions, deterministic builds

## Tips for Porting sbt Projects

1. Start with `bleep import` for automatic conversion
2. Move plugins to scripts in separate project
3. Replace sbt keys with constructor parameters
4. Use bleep's model types instead of sbt types
5. Test incrementally with `bleep compile`

## Debugging

- Scripts can be debugged in IDE like normal Scala programs
- Use `logger.warn()` for temporary debugging output
- Build model accessible via `started.build.exploded`
- Check normalized YAML with `bleep build show`

## Common Patterns

1. **Accessing Build Info in Scripts**
   ```scala
   val projects = started.build.exploded.projects
   val version = started.build.dynVer.version
   ```

2. **Running Commands**
   ```scala
   commands.compile(List(projectName))
   commands.test(List(projectName))
   ```

3. **Cross-Building**
   ```scala
   val crossProjects = started.build.exploded.projects.keys
     .filter(_.name.value == "mylib")
   ```

## When to Use Bleep

Bleep excels for:
- Projects wanting fast builds and IDE imports
- Teams frustrated with sbt's complexity
- Cross-platform Scala development (JVM/JS/Native)
- CI/CD pipelines needing fast startup

## Maintenance Process for Liberated Projects

This section documents the standard process for maintaining liberated sbt projects by merging upstream changes.

### Process Overview

When upstream sbt projects (like sbt-sonatype, sbt-ci-release) are updated, we need to periodically merge these changes into our bleep ports. This ensures we stay current with bug fixes, new features, and dependency updates.

### Step-by-Step Maintenance Process

1. **Navigate to the liberated project submodule**
   ```bash
   cd liberated/sbt-project-name
   ```

2. **Fetch latest upstream changes**
   ```bash
   git fetch origin-original
   ```

3. **Check and update dependencies in bleep.yaml**
   - Read the upstream `build.sbt` to identify dependency version changes
   - Update corresponding dependencies in main `bleep.yaml` for the bleep plugin
   - Example: If `sbt-sonatype` updates from 3.11.1 to 3.11.2, update `sbt-sonatype: 3.11.2` in bleep.yaml

4. **Merge upstream changes**
   ```bash
   git merge origin-original/main  # or master, depending on upstream
   ```

5. **Resolve conflicts**
   - Keep bleep-specific code (package names, imports, API adaptations)
   - Incorporate new upstream features and fixes
   - Remove sbt-specific code (AutoPlugin traits, sbt Keys, etc.)
   - Common conflict patterns:
     - Package declarations: Keep `bleep.plugin.*` structure
     - Imports: Keep bleep-specific imports, add new upstream dependencies
     - Class structure: Keep bleep constructor pattern, merge new methods

6. **Fix compilation errors**
   ```bash
   cd ../../  # Back to main bleep directory
   bleep compile bleep-plugin-project-name
   ```
   
   Common fixes needed:
   - Missing imports for new classes/types
   - API changes in dependencies
   - Java compatibility issues (e.g., `Path.of` → `Paths.get`)
   - Type signature changes

7. **Test the integration**
   ```bash
   bleep compile  # Test overall build
   bleep test bleep-tests  # Run tests if available
   ```

8. **Format code and commit the merge**
   ```bash
   cd ../../  # Back to main bleep directory
   bleep fmt  # Always format code before committing
   git add .
   git commit -m "Merge upstream changes from origin-original"
   ```

### Example: sbt-sonatype Maintenance (December 2024)

Successfully merged ~35 upstream commits including:

- **Sonatype Central Portal support**: New publishing endpoint replacing legacy OSSRH
- **Dependency updates**: Updated to sonatype-central-client 0.3.0, sttp 4.0.0-M16
- **New API routing**: Automatic detection of Central Portal vs legacy OSSRH based on credential host
- **Build improvements**: Cross-building for Scala 3, updated sbt plugins

Key changes implemented:
- Added `bundleRelease` helper with automatic routing logic
- Integrated Central Portal client libraries
- Maintained backward compatibility with legacy OSSRH
- Updated bleep plugin dependencies to match upstream versions

### Common Challenges and Solutions

1. **Import/Package Issues**
   - Problem: Scala compiler can't find types due to package restructuring
   - Solution: Verify imports match actual file locations, check `package` declarations

2. **sbt-Specific Code Removal**
   - Problem: Upstream adds new sbt-specific features
   - Solution: Adapt to bleep patterns (constructor injection, direct method calls)

3. **Dependency Version Conflicts**
   - Problem: New dependencies conflict with existing bleep dependencies
   - Solution: Update bleep.yaml dependencies, use `for3Use213: true` for compatibility

4. **API Breaking Changes**
   - Problem: Upstream changes method signatures or behavior
   - Solution: Update call sites in bleep-specific code, adapt to new APIs

### When to Skip Changes

Some upstream changes may not be relevant for bleep:
- sbt-specific build improvements
- sbt plugin architecture changes
- sbt task/setting modifications

Focus on merging:
- Core functionality improvements
- Bug fixes
- New features applicable to bleep
- Dependency updates
- Security fixes

### Documentation Updates

After successful maintenance:
1. Update this CLAUDE.md with any new patterns discovered
2. Note any breaking changes that affect bleep usage
3. Document new features available in the bleep port

This maintenance process ensures our liberated projects stay current while preserving the bleep-specific adaptations that make them work in our build system.

## Resources

- Documentation: `bleep-site-in/` directory
- Model definitions: `bleep-model/src/scala/bleep/model/`
- Example scripts: `scripts/src/scala/bleep/scripts/`
- Liberated projects: `liberated/*/` for real-world examples

## Developer Tips and Notes

- You can just compile with "bleep". To use that script you're referring to we need to create it with bleep setup-dev-script or something like that, and compile and then run. It's useful for testing bleep itself

## Design Principles

- **Never Use Default Parameters**: When defining methods or functions, always explicitly specify parameters