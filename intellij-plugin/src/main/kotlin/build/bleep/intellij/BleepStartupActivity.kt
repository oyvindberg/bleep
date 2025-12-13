package build.bleep.intellij

import com.intellij.notification.NotificationGroupManager
import com.intellij.notification.NotificationType
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.project.Project
import com.intellij.openapi.startup.ProjectActivity

/**
 * Runs when a project is opened to detect bleep projects and offer BSP setup.
 */
class BleepStartupActivity : ProjectActivity {
    private val LOG = Logger.getInstance(BleepStartupActivity::class.java)

    override suspend fun execute(project: Project) {
        val service = BleepService.getInstance(project)

        if (!service.isBleepProject) {
            LOG.debug("Not a bleep project: ${project.basePath}")
            return
        }

        // Initialize the BSP listener for this bleep project
        BleepBspListener.getInstance(project)

        LOG.info("Detected bleep project: ${project.basePath}")

        val config = service.bleepConfig
        if (config == null) {
            LOG.warn("Could not parse bleep.yaml")
            return
        }

        LOG.info("Bleep version: ${config.version}")

        // Check if BSP is already configured
        val bspFile = project.basePath?.let { java.io.File(it, ".bsp/bleep.json") }
        if (bspFile?.exists() == true) {
            service.bspStatus = BleepService.BspStatus.CONFIGURED
            LOG.info("BSP already configured")
            return
        }

        // Notify user that they can set up BSP
        showBleepDetectedNotification(project, config.version)
    }

    private fun showBleepDetectedNotification(project: Project, version: String) {
        val notification = NotificationGroupManager.getInstance()
            .getNotificationGroup("Bleep")
            .createNotification(
                "Bleep Project Detected",
                "This project uses bleep $version. Open the Bleep tool window to configure BSP import.",
                NotificationType.INFORMATION
            )
            .addAction(SetupBspNotificationAction())

        notification.notify(project)
    }
}

/**
 * Action shown in the notification to set up BSP.
 */
class SetupBspNotificationAction : com.intellij.notification.NotificationAction("Setup BSP") {
    override fun actionPerformed(e: com.intellij.openapi.actionSystem.AnActionEvent, notification: com.intellij.notification.Notification) {
        val project = e.project ?: return
        notification.expire()

        val service = BleepService.getInstance(project)
        service.setupIde { success, errorMessage ->
            if (success) {
                NotificationGroupManager.getInstance()
                    .getNotificationGroup("Bleep")
                    .createNotification(
                        "BSP Configured",
                        "Bleep BSP has been set up. Reload the project to import via BSP.",
                        NotificationType.INFORMATION
                    )
                    .notify(project)
            } else if (errorMessage != null) {
                NotificationGroupManager.getInstance()
                    .getNotificationGroup("Bleep")
                    .createNotification(
                        "BSP Setup Failed",
                        errorMessage,
                        NotificationType.ERROR
                    )
                    .notify(project)
            }
        }
    }
}
