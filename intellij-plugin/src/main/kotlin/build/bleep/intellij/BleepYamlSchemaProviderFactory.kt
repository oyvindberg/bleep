package build.bleep.intellij

import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.jetbrains.jsonSchema.extension.JsonSchemaFileProvider
import com.jetbrains.jsonSchema.extension.JsonSchemaProviderFactory
import com.jetbrains.jsonSchema.extension.SchemaType

/**
 * Provides JSON Schema for bleep.yaml files.
 */
class BleepYamlSchemaProviderFactory : JsonSchemaProviderFactory {
    override fun getProviders(project: Project): List<JsonSchemaFileProvider> {
        return listOf(BleepYamlSchemaProvider())
    }
}

class BleepYamlSchemaProvider : JsonSchemaFileProvider {
    override fun isAvailable(file: VirtualFile): Boolean {
        return file.name == "bleep.yaml"
    }

    override fun getName(): String = "Bleep Build"

    override fun getSchemaFile(): VirtualFile? {
        // The schema is referenced in the YAML file itself via $schema
        // This provider just ensures IntelliJ knows to look for it
        return null
    }

    override fun getSchemaType(): SchemaType = SchemaType.embeddedSchema

    override fun getRemoteSource(): String {
        return "https://raw.githubusercontent.com/oyvindberg/bleep/master/schema.json"
    }

    override fun isUserVisible(): Boolean = true
}

/**
 * Excludes bleep.yaml from other schema catalogs so our schema takes precedence.
 */
class BleepYamlSchemaExclusionProvider : com.jetbrains.jsonSchema.extension.JsonSchemaEnabler {
    override fun isEnabledForFile(file: VirtualFile, project: Project?): Boolean {
        return file.name == "bleep.yaml"
    }

    override fun canBeSchemaFile(file: VirtualFile): Boolean {
        return file.name.endsWith(".json")
    }
}
