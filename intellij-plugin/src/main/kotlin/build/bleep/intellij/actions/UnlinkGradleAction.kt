package build.bleep.intellij.actions

import build.bleep.intellij.BleepService
import com.intellij.notification.NotificationGroupManager
import com.intellij.notification.NotificationType
import com.intellij.openapi.actionSystem.AnAction
import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.externalSystem.model.ProjectSystemId
import com.intellij.openapi.externalSystem.service.project.manage.ExternalProjectsManagerImpl
import com.intellij.openapi.externalSystem.util.ExternalSystemApiUtil
import com.intellij.openapi.externalSystem.util.ExternalSystemUtil
import com.intellij.openapi.externalSystem.ExternalSystemModulePropertyManager
import com.intellij.openapi.module.ModuleManager

/**
 * Action to unlink other build tools from a bleep project and set up BSP instead.
 */
class UnlinkGradleAction : AnAction(
    "Switch to Bleep BSP",
    "Unlink other build tools (Gradle, Maven, sbt) and set up Bleep BSP",
    null
) {
    private val LOG = Logger.getInstance(UnlinkGradleAction::class.java)

    // Build systems to unlink (not BSP - we want to keep that)
    private val BUILD_SYSTEMS_TO_UNLINK = listOf(
        ProjectSystemId("GRADLE"),
        ProjectSystemId("Maven"),
        ProjectSystemId("SBT")
    )

    override fun actionPerformed(e: AnActionEvent) {
        val project = e.project ?: return
        val projectPath = project.basePath ?: return

        LOG.info("Attempting to unlink build tools from project: ${project.name}")

        try {
            val manager = ExternalProjectsManagerImpl.getInstance(project)

            // First try to unlink via ExternalSystemSettings (this modifies .idea/*.xml)
            for (systemId in BUILD_SYSTEMS_TO_UNLINK) {
                try {
                    val settings = ExternalSystemApiUtil.getSettings(project, systemId)
                    val linkedProjects = settings.linkedProjectsSettings.toList()
                    LOG.info("Found ${linkedProjects.size} linked ${systemId.id} projects")

                    for (linkedProject in linkedProjects) {
                        val linkedPath = linkedProject.externalProjectPath
                        LOG.info("Unlinking ${systemId.id} project at: $linkedPath")
                        settings.unlinkExternalProject(linkedPath)
                    }
                } catch (ex: Exception) {
                    LOG.debug("Could not access settings for ${systemId.id}: ${ex.message}")
                }
            }

            // Also forget external project data (clears cached data)
            for (systemId in BUILD_SYSTEMS_TO_UNLINK) {
                try {
                    manager.forgetExternalProjectData(systemId, projectPath)
                    LOG.info("Forgot cached data for ${systemId.id}")
                } catch (ex: Exception) {
                    LOG.debug("Could not forget data for ${systemId.id}: ${ex.message}")
                }
            }

            // Remove modules created by the external systems
            val moduleManager = ModuleManager.getInstance(project)
            val modulesToRemove = mutableListOf<com.intellij.openapi.module.Module>()

            for (module in moduleManager.modules) {
                // Check if module was created by one of the systems we're unlinking
                val propManager = ExternalSystemModulePropertyManager.getInstance(module)
                val externalSystemId = propManager.getExternalSystemId()
                if (externalSystemId != null && BUILD_SYSTEMS_TO_UNLINK.any { it.id == externalSystemId }) {
                    LOG.info("Marking module for removal: ${module.name} (system: $externalSystemId)")
                    modulesToRemove.add(module)
                }
            }

            if (modulesToRemove.isNotEmpty()) {
                LOG.info("Removing ${modulesToRemove.size} modules from external systems")
                ApplicationManager.getApplication().runWriteAction {
                    val modifiableModel = moduleManager.getModifiableModel()
                    for (module in modulesToRemove) {
                        modifiableModel.disposeModule(module)
                    }
                    modifiableModel.commit()
                }
                LOG.info("Modules removed successfully")
            }

            // Schedule UI update
            for (systemId in BUILD_SYSTEMS_TO_UNLINK) {
                try {
                    ExternalSystemUtil.scheduleExternalViewStructureUpdate(project, systemId)
                } catch (ex: Exception) {
                    LOG.debug("Could not schedule update for ${systemId.id}: ${ex.message}")
                }
            }

            LOG.info("Unlink attempt completed")

            // Now set up BSP
            val service = BleepService.getInstance(project)
            service.setupIde { success, errorMessage ->
                if (success) {
                    NotificationGroupManager.getInstance()
                        .getNotificationGroup("Bleep")
                        .createNotification(
                            "Switched to Bleep BSP",
                            "Other build tools unlinked. BSP is now configured. Please reload the project to use BSP.",
                            NotificationType.INFORMATION
                        )
                        .notify(project)
                } else {
                    NotificationGroupManager.getInstance()
                        .getNotificationGroup("Bleep")
                        .createNotification(
                            "BSP Setup Issue",
                            "BSP setup failed: ${errorMessage ?: "unknown error"}",
                            NotificationType.WARNING
                        )
                        .notify(project)
                }
            }
        } catch (ex: Exception) {
            LOG.error("Failed to switch to BSP", ex)
            NotificationGroupManager.getInstance()
                .getNotificationGroup("Bleep")
                .createNotification(
                    "Failed to Switch Build Tool",
                    ex.message ?: "Unknown error",
                    NotificationType.ERROR
                )
                .notify(project)
        }
    }

    override fun update(e: AnActionEvent) {
        val project = e.project
        if (project == null) {
            e.presentation.isEnabledAndVisible = false
            return
        }

        // Only show if this is a bleep project
        val service = BleepService.getInstance(project)
        e.presentation.isEnabledAndVisible = service.isBleepProject
    }
}
