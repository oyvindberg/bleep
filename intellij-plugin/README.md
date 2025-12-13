# Bleep IntelliJ Plugin

IntelliJ IDEA integration for the [Bleep](https://bleep.build) Java and Scala build tool.

## Features

- Auto-detect bleep projects when opening directories with `bleep.yaml`
- Automatic bleep binary download (correct version for your OS/arch)
- Project selector for choosing which projects to mount in BSP
- Dedicated Bleep tool window for BSP status and controls
- JSON Schema support for `bleep.yaml` (autocomplete, validation)

## Development

### Prerequisites

- JDK 21
- IntelliJ IDEA (for testing)

### Build

```bash
cd intellij-plugin
./gradlew buildPlugin
```

The built plugin ZIP will be at: `build/distributions/bleep-intellij-plugin-X.X.X.zip`

### Run/Debug in IDE

Launch a sandboxed IntelliJ instance with the plugin loaded:

```bash
./gradlew runIde
```

This opens a fresh IntelliJ where you can test the plugin. Open a bleep project to test.

### Install from ZIP

1. Build the plugin: `./gradlew buildPlugin`
2. In IntelliJ: **Settings** → **Plugins** → **⚙️** → **Install Plugin from Disk...**
3. Select `build/distributions/bleep-intellij-plugin-X.X.X.zip`
4. Restart IntelliJ

### Reinstall after changes

```bash
./gradlew buildPlugin
# Then in IntelliJ: Settings → Plugins → ⚙️ → Install Plugin from Disk...
# Restart IntelliJ
```

Or for faster iteration during development:

```bash
./gradlew runIde
```

### Clean build

```bash
./gradlew clean buildPlugin
```

## Architecture

```
src/main/kotlin/build/bleep/intellij/
├── BleepConfig.kt              # Parse $version from bleep.yaml
├── BleepDownloader.kt          # Download bleep binary (like FetchBleepRelease.scala)
├── BleepIcons.kt               # Plugin icons
├── BleepProjectOpenProcessor.kt # Detect bleep projects on open
├── BleepService.kt             # Project-level service, calls bleep CLI
├── BleepStartupActivity.kt     # Show notification when opening bleep project
├── BleepToolWindowFactory.kt   # Tool window with project selector
├── BleepYamlSchemaProviderFactory.kt # JSON Schema for bleep.yaml
└── actions/
    ├── RefreshBspAction.kt     # Refresh BSP connection
    └── SetupIdeAction.kt       # Run bleep setup-ide
```

## How it works

1. **Detection**: When you open a directory, `BleepProjectOpenProcessor` checks for `bleep.yaml`
2. **Version**: `BleepConfig` parses just the `$version` field from `bleep.yaml`
3. **Download**: `BleepDownloader` fetches the correct bleep native binary to IntelliJ's cache
4. **Projects**: `BleepService` calls `bleep --no-color projects` to get the actual project list
5. **Setup**: User selects projects in tool window, clicks "Setup IDE" which runs `bleep setup-ide [projects...]`
6. **BSP**: The Scala plugin then picks up `.bsp/bleep.json` and imports via BSP

## Dependencies

- Requires: Scala plugin, YAML plugin
- Target: IntelliJ IDEA 2024.3+
