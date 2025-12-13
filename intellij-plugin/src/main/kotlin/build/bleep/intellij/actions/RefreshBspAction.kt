package build.bleep.intellij.actions

import build.bleep.intellij.BleepService
import com.intellij.openapi.actionSystem.AnAction
import com.intellij.openapi.actionSystem.AnActionEvent

/**
 * Action to refresh the BSP connection by re-running setup-ide.
 */
class RefreshBspAction : AnAction() {
    override fun actionPerformed(e: AnActionEvent) {
        val project = e.project ?: return
        val service = BleepService.getInstance(project)

        service.setupIde { _, _ ->
            // Status will be updated by setupIde
        }
    }

    override fun update(e: AnActionEvent) {
        val project = e.project
        e.presentation.isEnabledAndVisible = project != null &&
                BleepService.getInstance(project).isBleepProject
    }
}
