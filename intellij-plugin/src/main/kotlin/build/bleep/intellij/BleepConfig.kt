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
    val version: String
) {
    companion object {
        private val LOG = Logger.getInstance(BleepConfig::class.java)

        fun parse(bleepYamlFile: File): BleepConfig? {
            if (!bleepYamlFile.exists()) return null

            return try {
                val yaml = Yaml()
                val data = FileReader(bleepYamlFile).use { reader ->
                    @Suppress("UNCHECKED_CAST")
                    yaml.load<Map<String, Any>>(reader) as? Map<String, Any>
                } ?: return null

                val version = (data["\$version"] as? String) ?: return null

                BleepConfig(version = version)
            } catch (e: Exception) {
                LOG.warn("Failed to parse bleep.yaml", e)
                null
            }
        }

        fun forDirectory(dir: File): BleepConfig? = parse(File(dir, "bleep.yaml"))
        fun forDirectory(dir: Path): BleepConfig? = forDirectory(dir.toFile())

        fun isBleepProject(dir: File): Boolean = File(dir, "bleep.yaml").exists()
    }
}
