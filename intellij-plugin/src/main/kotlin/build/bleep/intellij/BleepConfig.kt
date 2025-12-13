package build.bleep.intellij

import com.intellij.openapi.diagnostic.Logger
import org.yaml.snakeyaml.Yaml
import java.io.File
import java.io.FileReader
import java.nio.file.Path

/**
 * Minimal bleep.yaml parser - just extracts the version.
 * For project lists and other complex info, we call the bleep binary
 * since it handles templates, cross-compilation, etc.
 */
data class BleepConfig(
    val version: String,
    val file: File
) {
    companion object {
        private val LOG = Logger.getInstance(BleepConfig::class.java)

        /** Minimum bleep version required for extract-info commands */
        const val MINIMUM_VERSION = "0.0.14"

        fun parse(bleepYamlFile: File): BleepConfig? {
            if (!bleepYamlFile.exists()) return null

            return try {
                val yaml = Yaml()
                val data = FileReader(bleepYamlFile).use { reader ->
                    @Suppress("UNCHECKED_CAST")
                    yaml.load<Map<String, Any>>(reader) as? Map<String, Any>
                } ?: return null

                val version = (data["\$version"] as? String) ?: return null

                BleepConfig(version = version, file = bleepYamlFile)
            } catch (e: Exception) {
                LOG.warn("Failed to parse bleep.yaml", e)
                null
            }
        }

        fun forDirectory(dir: File): BleepConfig? = parse(File(dir, "bleep.yaml"))
        fun forDirectory(dir: Path): BleepConfig? = forDirectory(dir.toFile())

        fun isBleepProject(dir: File): Boolean = File(dir, "bleep.yaml").exists()

        /**
         * Compare two version strings (e.g., "0.0.13" vs "0.0.14").
         * Returns negative if v1 < v2, positive if v1 > v2, 0 if equal.
         */
        fun compareVersions(v1: String, v2: String): Int {
            val parts1 = v1.split(".").map { it.toIntOrNull() ?: 0 }
            val parts2 = v2.split(".").map { it.toIntOrNull() ?: 0 }
            val maxLen = maxOf(parts1.size, parts2.size)

            for (i in 0 until maxLen) {
                val p1 = parts1.getOrElse(i) { 0 }
                val p2 = parts2.getOrElse(i) { 0 }
                if (p1 != p2) return p1 - p2
            }
            return 0
        }
    }

    /**
     * Check if this version meets the minimum requirement.
     */
    fun meetsMinimumVersion(): Boolean {
        return compareVersions(version, MINIMUM_VERSION) >= 0
    }

    /**
     * Update the $version field in bleep.yaml to the new version.
     * Returns true if successful.
     */
    fun updateVersion(newVersion: String): Boolean {
        return try {
            val content = file.readText()
            // Replace the $version line, handling both quoted and unquoted versions
            val updatedContent = content.replace(
                Regex("""(\${'$'}version:\s*)["']?[\d.]+["']?"""),
                "$1$newVersion"
            )
            if (content == updatedContent) {
                LOG.warn("Failed to find \$version line in ${file.absolutePath}")
                return false
            }
            file.writeText(updatedContent)
            LOG.info("Updated bleep version from $version to $newVersion in ${file.absolutePath}")
            true
        } catch (e: Exception) {
            LOG.error("Failed to update bleep version", e)
            false
        }
    }
}
