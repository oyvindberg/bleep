package build.bleep.intellij

import com.intellij.ide.impl.OpenProjectTask
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.project.Project
import com.intellij.openapi.project.ex.ProjectManagerEx
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.projectImport.ProjectOpenProcessor
import javax.swing.Icon
import java.nio.file.Path

/**
 * Detects bleep projects when opening directories containing bleep.yaml
 */
class BleepProjectOpenProcessor : ProjectOpenProcessor() {
    private val LOG = Logger.getInstance(BleepProjectOpenProcessor::class.java)

    override val name: String = "Bleep"

    override fun getIcon(file: VirtualFile): Icon? = BleepIcons.BLEEP

    override fun canOpenProject(file: VirtualFile): Boolean {
        if (!file.isDirectory) return false
        val bleepYaml = file.findChild("bleep.yaml")
        val result = bleepYaml != null && bleepYaml.exists()
        if (result) {
            LOG.info("Detected bleep project at ${file.path}")
        }
        return result
    }

    @Suppress("OVERRIDE_DEPRECATION")
    override fun doOpenProject(
        virtualFile: VirtualFile,
        projectToClose: Project?,
        forceOpenInNewFrame: Boolean
    ): Project? {
        LOG.info("Opening bleep project at ${virtualFile.path}")

        val projectPath = Path.of(virtualFile.path)
        val options = OpenProjectTask {
            this.forceOpenInNewFrame = forceOpenInNewFrame
            this.projectToClose = projectToClose
        }

        val project = ProjectManagerEx.getInstanceEx().openProject(projectPath, options)

        if (project != null) {
            LOG.info("Bleep project opened successfully: ${project.name}")
        } else {
            LOG.warn("Failed to open bleep project at ${virtualFile.path}")
        }

        return project
    }
}
