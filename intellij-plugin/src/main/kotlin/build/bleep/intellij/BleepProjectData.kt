package build.bleep.intellij

import com.google.gson.Gson
import com.google.gson.annotations.SerializedName
import com.intellij.openapi.diagnostic.Logger
import java.io.BufferedReader
import java.io.File
import java.io.InputStreamReader

/**
 * Data classes for parsing bleep extract-info JSON output.
 *
 * Note: Bleep distinguishes between:
 * - CrossProjectName: Full name like "typo@jvm3" (project with cross-build suffix)
 * - ProjectName: Base name like "typo" (the name without cross-build suffix)
 *
 * Commands that operate on project structure (rename, merge, move) use ProjectName.
 * Commands that operate on build targets (compile, test, diff) use CrossProjectName.
 */
data class ProjectInfo(
    val name: String,  // This is the CrossProjectName (e.g., "typo@jvm3")
    val dependsOn: List<String>,
    val isTest: Boolean
) {
    /**
     * The base project name without the cross-build suffix.
     * e.g., "typo@jvm3" -> "typo", "myproject" -> "myproject"
     */
    val projectName: String
        get() = name.substringBefore('@')

    /**
     * The cross-build suffix, if any.
     * e.g., "typo@jvm3" -> "jvm3", "myproject" -> null
     */
    val crossId: String?
        get() = if (name.contains('@')) name.substringAfter('@') else null
}

data class GroupInfo(
    val name: String,
    val projects: List<String>
)

data class ProjectGraphOutput(
    val projects: List<ProjectInfo>
)

data class ProjectGroupsOutput(
    val groups: List<GroupInfo>
)

data class ScriptInfo(
    val name: String,
    val project: String,
    val mainClass: String
)

data class ScriptsOutput(
    val scripts: List<ScriptInfo>
)

data class SourceGenInfo(
    val project: String,
    val sourceGenProject: String,
    val mainClass: String
)

data class SourceGenOutput(
    val sourcegens: List<SourceGenInfo>
)

/**
 * Aggregated output from extract-info all command.
 * Contains all project data in a single response.
 */
data class AllOutput(
    val projects: List<ProjectInfo>,
    val groups: List<GroupInfo>,
    val scripts: List<ScriptInfo>,
    val sourcegens: List<SourceGenInfo>
)

/**
 * Fetches project data from bleep extract-info commands.
 */
object BleepProjectData {
    private val LOG = Logger.getInstance(BleepProjectData::class.java)
    private val gson = Gson()

    // TODO: Remove this hardcoded path after testing
    // Set to null to use the downloaded bleep binary
    var hardcodedBleepPath: String? = "/Users/oyvind/bleep/bleep-cli@jvm3.sh"

    /**
     * Fetches all project data in a single bleep command.
     * This is more efficient than calling individual fetch methods.
     */
    fun fetchAll(bleepPath: File, projectDir: File): AllOutput? {
        val effectivePath = hardcodedBleepPath?.let { File(it) } ?: bleepPath
        val json = runBleepCommand(effectivePath, projectDir, "extract-info", "all")
        return json?.let {
            try {
                gson.fromJson(it, AllOutput::class.java)
            } catch (e: Exception) {
                LOG.warn("Failed to parse extract-info all JSON", e)
                null
            }
        }
    }

    private fun runBleepCommand(bleepPath: File, projectDir: File, vararg args: String): String? {
        if (!bleepPath.exists()) {
            LOG.warn("Bleep binary not found: $bleepPath")
            return null
        }

        return try {
            val command = listOf(bleepPath.absolutePath, "--no-color", "-d", projectDir.absolutePath) + args.toList()
            LOG.info("Running: ${command.joinToString(" ")}")

            val process = ProcessBuilder(command)
                .directory(projectDir)
                .redirectErrorStream(false)
                .start()

            val output = StringBuilder()
            val errors = StringBuilder()

            // Read stdout - looking for JSON line
            BufferedReader(InputStreamReader(process.inputStream)).use { reader ->
                var line: String?
                while (reader.readLine().also { line = it } != null) {
                    val l = line ?: continue
                    // JSON output starts with { and doesn't have log prefixes
                    if (l.trimStart().startsWith("{")) {
                        output.append(l)
                    }
                }
            }

            // Read stderr
            BufferedReader(InputStreamReader(process.errorStream)).use { reader ->
                var line: String?
                while (reader.readLine().also { line = it } != null) {
                    errors.appendLine(line)
                }
            }

            val exitCode = process.waitFor()
            if (exitCode != 0) {
                LOG.warn("Bleep command failed with exit code $exitCode: $errors")
                return null
            }

            val result = output.toString()
            if (result.isEmpty()) {
                LOG.warn("No JSON output from bleep command")
                null
            } else {
                result
            }
        } catch (e: Exception) {
            LOG.error("Failed to run bleep command", e)
            null
        }
    }
}
