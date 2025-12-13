package build.bleep.intellij

import com.intellij.notification.NotificationGroupManager
import com.intellij.notification.NotificationType
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.components.Service
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.project.Project
import com.intellij.openapi.Disposable
import com.intellij.openapi.vfs.VirtualFileManager
import com.intellij.openapi.vfs.newvfs.BulkFileListener
import com.intellij.openapi.vfs.newvfs.events.VFileEvent
import com.intellij.util.Alarm

/**
 * Listener for bleep.yaml file changes.
 * Automatically triggers BSP sync when the build file is modified.
 */
@Service(Service.Level.PROJECT)
class BleepBspListener(private val project: Project) : Disposable {
    private val LOG = Logger.getInstance(BleepBspListener::class.java)

    // Debounce alarm to avoid triggering multiple syncs for rapid file changes
    private val syncAlarm = Alarm(Alarm.ThreadToUse.POOLED_THREAD, this)
    private val DEBOUNCE_DELAY_MS = 1000 // Wait 1 second after last change

    init {
        setupListeners()
    }

    private fun setupListeners() {
        val connection = project.messageBus.connect(this)

        // Listen for bleep.yaml file changes
        connection.subscribe(VirtualFileManager.VFS_CHANGES, object : BulkFileListener {
            override fun after(events: List<VFileEvent>) {
                for (event in events) {
                    val file = event.file ?: continue
                    if (file.name == "bleep.yaml") {
                        // Check if file is within this project
                        val projectPath = project.basePath ?: continue
                        if (file.path.startsWith(projectPath)) {
                            LOG.info("bleep.yaml changed, scheduling BSP sync")
                            scheduleBspSync()
                            return // Only need to trigger once per batch of events
                        }
                    }
                }
            }
        })

        LOG.info("BleepBspListener initialized for project: ${project.name}")
    }

    private fun scheduleBspSync() {
        // Cancel any pending sync and schedule a new one (debounce)
        syncAlarm.cancelAllRequests()
        syncAlarm.addRequest({
            ApplicationManager.getApplication().invokeLater {
                triggerBspSync()
            }
        }, DEBOUNCE_DELAY_MS)
    }

    private fun triggerBspSync() {
        if (project.isDisposed) return

        LOG.info("Triggering BSP sync due to bleep.yaml change")

        // Show notification
        NotificationGroupManager.getInstance()
            .getNotificationGroup("Bleep")
            .createNotification(
                "Bleep Build Changed",
                "Syncing BSP project...",
                NotificationType.INFORMATION
            )
            .notify(project)

        // Trigger BSP refresh
        val service = BleepService.getInstance(project)
        service.triggerBspSync()
    }

    companion object {
        fun getInstance(project: Project): BleepBspListener {
            return project.getService(BleepBspListener::class.java)
        }
    }

    override fun dispose() {
        syncAlarm.cancelAllRequests()
        LOG.info("BleepBspListener disposed")
    }
}
