package build.bleep.intellij

import com.intellij.openapi.Disposable
import com.intellij.openapi.actionSystem.ActionManager
import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.actionSystem.DataContext
import com.intellij.openapi.actionSystem.Presentation
import com.intellij.openapi.components.Service
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.externalSystem.model.ProjectSystemId
import com.intellij.openapi.externalSystem.service.project.ExternalProjectRefreshCallback
import com.intellij.openapi.externalSystem.util.ExternalSystemUtil
import com.intellij.openapi.externalSystem.importing.ImportSpecBuilder
import com.intellij.openapi.externalSystem.service.execution.ProgressExecutionMode
import com.intellij.openapi.progress.ProgressIndicator
import com.intellij.openapi.progress.ProgressManager
import com.intellij.openapi.progress.Task
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.LocalFileSystem
import com.intellij.openapi.vfs.VfsUtil
import java.io.BufferedReader
import java.io.File
import java.io.InputStreamReader
import java.util.concurrent.ConcurrentHashMap

/**
 * Project-level service for managing bleep state and operations.
 */
@Service(Service.Level.PROJECT)
class BleepService(private val project: Project) : Disposable {
    private val LOG = Logger.getInstance(BleepService::class.java)

    private var bleepExecutable: File? = null
    private var config: BleepConfig? = null
    private var cachedAllData: AllOutput? = null

    /** Last error message from ensureBleep, for UI display */
    var lastBleepError: String? = null
        private set
    private val selectedProjects = ConcurrentHashMap.newKeySet<String>()

    // Listeners for config changes (tool window refresh)
    private val configChangeListeners = mutableListOf<() -> Unit>()

    fun addConfigChangeListener(listener: () -> Unit) {
        configChangeListeners.add(listener)
    }

    fun removeConfigChangeListener(listener: () -> Unit) {
        configChangeListeners.remove(listener)
    }

    private fun notifyConfigChanged() {
        configChangeListeners.forEach { it() }
    }

    enum class BspStatus {
        NOT_CONFIGURED, CONFIGURED, CONNECTED, ERROR
    }

    var bspStatus: BspStatus = BspStatus.NOT_CONFIGURED

    companion object {
        // BSP system ID used by the Scala plugin
        val BSP_SYSTEM_ID = ProjectSystemId("BSP", "BSP")

        fun getInstance(project: Project): BleepService {
            return project.getService(BleepService::class.java)
        }
    }

    val projectBasePath: File? get() = project.basePath?.let { File(it) }

    val bleepConfig: BleepConfig?
        get() {
            if (config == null) {
                config = projectBasePath?.let { BleepConfig.forDirectory(it) }
            }
            return config
        }

    val isBleepProject: Boolean
        get() = projectBasePath?.let { BleepConfig.isBleepProject(it) } ?: false

    fun reloadConfig() {
        config = projectBasePath?.let { BleepConfig.forDirectory(it) }
        cachedAllData = null
    }

    /**
     * Load selected projects from .bleep/conf/bsp-project-selection.yaml
     */
    fun loadProjectSelection(): Set<String> {
        val basePath = projectBasePath ?: return emptySet()
        val selectionFile = File(basePath, ".bleep/conf/bsp-project-selection.yaml")

        if (!selectionFile.exists()) {
            LOG.info("No project selection file found at $selectionFile")
            return emptySet()
        }

        return try {
            val yaml = org.yaml.snakeyaml.Yaml()
            val content = selectionFile.readText()
            val parsed = yaml.load<Any>(content)

            when (parsed) {
                is List<*> -> parsed.filterIsInstance<String>().toSet()
                else -> {
                    LOG.warn("Unexpected format in bsp-project-selection.yaml")
                    emptySet()
                }
            }
        } catch (e: Exception) {
            LOG.warn("Failed to load project selection from $selectionFile", e)
            emptySet()
        }
    }

    /**
     * Save selected projects to .bleep/conf/bsp-project-selection.yaml
     */
    fun saveProjectSelection() {
        val basePath = projectBasePath ?: return
        val confDir = File(basePath, ".bleep/conf")
        val selectionFile = File(confDir, "bsp-project-selection.yaml")

        try {
            if (!confDir.exists()) {
                confDir.mkdirs()
            }

            val projects = selectedProjects.toList().sorted()
            val yaml = org.yaml.snakeyaml.Yaml()
            selectionFile.writeText(yaml.dump(projects))
            LOG.info("Saved ${projects.size} projects to $selectionFile")
        } catch (e: Exception) {
            LOG.warn("Failed to save project selection to $selectionFile", e)
        }
    }

    fun getSelectedProjects(): Set<String> = selectedProjects.toSet()

    fun setSelectedProjects(projects: Set<String>) {
        selectedProjects.clear()
        selectedProjects.addAll(projects)
    }

    fun toggleProjectSelection(projectName: String) {
        if (projectName in selectedProjects) {
            selectedProjects.remove(projectName)
        } else {
            selectedProjects.add(projectName)
        }
    }

    fun selectGroup(groupProjects: List<String>) {
        selectedProjects.addAll(groupProjects)
    }

    fun deselectGroup(groupProjects: List<String>) {
        selectedProjects.removeAll(groupProjects.toSet())
    }

    /**
     * Result of version check.
     */
    sealed class VersionCheckResult {
        object Ok : VersionCheckResult()
        data class TooOld(val current: String, val required: String) : VersionCheckResult()
        object NoConfig : VersionCheckResult()
    }

    /**
     * Result of ensuring bleep is available.
     */
    sealed class BleepResult {
        data class Success(val executable: File) : BleepResult()
        data class Error(val message: String) : BleepResult()
    }

    /**
     * Check if the bleep version meets the minimum requirement.
     */
    fun checkVersion(): VersionCheckResult {
        val config = bleepConfig ?: return VersionCheckResult.NoConfig
        return if (config.meetsMinimumVersion()) {
            VersionCheckResult.Ok
        } else {
            VersionCheckResult.TooOld(config.version, BleepConfig.MINIMUM_VERSION)
        }
    }

    /**
     * Bump the bleep version in bleep.yaml to the minimum required version.
     * Returns true if successful.
     */
    fun bumpVersion(): Boolean {
        val config = bleepConfig ?: return false
        val success = config.updateVersion(BleepConfig.MINIMUM_VERSION)
        if (success) {
            reloadConfig()
            // Clear cached executable since version changed
            bleepExecutable = null
        }
        return success
    }

    /**
     * Ensures bleep is downloaded and returns the path to the executable.
     * Returns BleepResult with specific error message if it fails.
     */
    fun ensureBleep(indicator: ProgressIndicator? = null): BleepResult {
        if (bleepExecutable?.exists() == true) {
            return BleepResult.Success(bleepExecutable!!)
        }

        val config = bleepConfig
            ?: return BleepResult.Error("No bleep.yaml found in project")

        // Check version requirement
        if (!config.meetsMinimumVersion()) {
            LOG.warn("Bleep version ${config.version} is too old, need ${BleepConfig.MINIMUM_VERSION}")
            return BleepResult.Error("Bleep version ${config.version} is too old (requires ${BleepConfig.MINIMUM_VERSION})")
        }

        val version = config.version

        // Check cache first
        val cached = BleepDownloader.getCachedBleep(version)
        if (cached != null) {
            bleepExecutable = cached
            return BleepResult.Success(cached)
        }

        // Download
        return try {
            val downloaded = BleepDownloader.downloadBleep(version, indicator)
            bleepExecutable = downloaded
            BleepResult.Success(downloaded)
        } catch (e: Exception) {
            LOG.warn("Failed to download bleep $version: ${e.message}")
            val errorMsg = when {
                e.message?.contains("404") == true || e.message?.contains("Not Found") == true ->
                    "Bleep version $version not found. This version may not be released yet."
                e.message?.contains("connect") == true || e.message?.contains("timeout") == true ->
                    "Failed to download bleep $version: Network error (${e.message})"
                else ->
                    "Failed to download bleep $version: ${e.message}"
            }
            BleepResult.Error(errorMsg)
        }
    }

    /**
     * Runs bleep setup-ide with the selected projects.
     * Callback receives (success, errorMessage).
     */
    fun setupIde(onComplete: (Boolean, String?) -> Unit) {
        // Save selection first
        saveProjectSelection()

        ProgressManager.getInstance().run(object : Task.Backgroundable(project, "Setting up Bleep IDE", true) {
            override fun run(indicator: ProgressIndicator) {
                indicator.text = "Ensuring bleep is available..."
                val bleepResult = ensureBleep(indicator)
                val bleep = when (bleepResult) {
                    is BleepResult.Success -> bleepResult.executable
                    is BleepResult.Error -> {
                        onComplete(false, bleepResult.message)
                        return
                    }
                }

                indicator.text = "Validating project selection..."

                // Get valid project names from the build to filter out stale selections
                val validProjectNames = getCrossProjectNames(indicator).toSet()
                val validatedProjects = if (selectedProjects.isNotEmpty() && validProjectNames.isNotEmpty()) {
                    val valid = selectedProjects.filter { it in validProjectNames }
                    val invalid = selectedProjects.filter { it !in validProjectNames }
                    if (invalid.isNotEmpty()) {
                        LOG.warn("Removing ${invalid.size} stale project selections: ${invalid.take(5)}...")
                        // Update the stored selection to remove invalid projects
                        setSelectedProjects(valid.toSet())
                        saveProjectSelection()
                    }
                    valid
                } else {
                    selectedProjects.toList()
                }

                indicator.text = "Running bleep setup-ide..."

                val projectArgs = validatedProjects

                val basePath = projectBasePath
                if (basePath == null) {
                    onComplete(false, "Project base path not found")
                    return
                }

                val cmd = mutableListOf(bleep.absolutePath, "--no-color", "-d", basePath.absolutePath, "setup-ide")
                cmd.addAll(projectArgs)

                LOG.info("Running setup-ide with ${projectArgs.size} projects: ${projectArgs.take(5)}...")

                val result = runBleepCommandWithOutput(cmd, indicator)
                if (result.success) {
                    bspStatus = BspStatus.CONFIGURED
                    indicator.text = "Importing BSP project..."
                    // Trigger BSP import on the EDT
                    com.intellij.openapi.application.ApplicationManager.getApplication().invokeLater {
                        refreshBsp { bspSuccess, bspError ->
                            if (bspSuccess) {
                                onComplete(true, null)
                            } else {
                                onComplete(false, bspError ?: "BSP import failed")
                            }
                        }
                    }
                } else {
                    bspStatus = BspStatus.ERROR
                    onComplete(false, result.errorMessage ?: "bleep setup-ide failed")
                }
            }
        })
    }

    /**
     * Result of running a bleep command.
     */
    data class CommandResult(
        val success: Boolean,
        val output: String,
        val errorMessage: String?
    )

    /**
     * Loads all project data in a single bleep command.
     * This is more efficient than loading data individually.
     */
    private fun loadAllData(indicator: ProgressIndicator? = null): AllOutput? {
        cachedAllData?.let { return it }

        val bleepResult = ensureBleep(indicator)
        val bleep = when (bleepResult) {
            is BleepResult.Success -> {
                lastBleepError = null
                bleepResult.executable
            }
            is BleepResult.Error -> {
                lastBleepError = bleepResult.message
                return null
            }
        }
        val basePath = projectBasePath ?: return null

        val data = BleepProjectData.fetchAll(bleep, basePath)
        cachedAllData = data
        return data
    }

    /**
     * Get the project graph using bleep extract-info all.
     */
    fun getProjectGraph(indicator: ProgressIndicator? = null): ProjectGraphOutput? {
        val data = loadAllData(indicator) ?: return null
        return ProjectGraphOutput(data.projects)
    }

    /**
     * Get the project groups using bleep extract-info all.
     */
    fun getProjectGroups(indicator: ProgressIndicator? = null): ProjectGroupsOutput? {
        val data = loadAllData(indicator) ?: return null
        return ProjectGroupsOutput(data.groups)
    }

    /**
     * Get cross-project names (full names like "typo@jvm3").
     * Use for commands that operate on build targets: compile, test, diff, create-directories, show exploded/bloop.
     */
    fun getCrossProjectNames(indicator: ProgressIndicator? = null): List<String> {
        val graph = getProjectGraph(indicator) ?: return emptyList()
        return graph.projects.map { it.name }.sorted()
    }

    /**
     * Get distinct project names (base names like "typo" without cross suffix).
     * Use for commands that operate on project structure: project-rename, project-merge-into, projects-move, show short.
     */
    fun getProjectNames(indicator: ProgressIndicator? = null): List<String> {
        val graph = getProjectGraph(indicator) ?: return emptyList()
        return graph.projects.map { it.projectName }.distinct().sorted()
    }

    /**
     * Legacy method - get project names as simple list.
     * @deprecated Use getCrossProjectNames() or getProjectNames() instead for clarity
     */
    @Deprecated("Use getCrossProjectNames() or getProjectNames() instead", ReplaceWith("getCrossProjectNames(indicator)"))
    fun getProjects(indicator: ProgressIndicator? = null): List<String> {
        return getCrossProjectNames(indicator)
    }

    /**
     * Get the scripts using bleep extract-info all.
     */
    fun getScripts(indicator: ProgressIndicator? = null): ScriptsOutput? {
        val data = loadAllData(indicator) ?: return null
        return ScriptsOutput(data.scripts)
    }

    /**
     * Get the sourcegen definitions using bleep extract-info all.
     */
    fun getSourceGen(indicator: ProgressIndicator? = null): SourceGenOutput? {
        val data = loadAllData(indicator) ?: return null
        return SourceGenOutput(data.sourcegens)
    }

    /**
     * Runs a bleep command and returns whether it succeeded.
     */
    private fun runBleepCommand(command: List<String>, indicator: ProgressIndicator): Boolean {
        return runBleepCommandWithOutput(command, indicator).success
    }

    /**
     * Runs a bleep command and returns the result with output.
     */
    private fun runBleepCommandWithOutput(command: List<String>, indicator: ProgressIndicator): CommandResult {
        val basePath = projectBasePath
            ?: return CommandResult(false, "", "Project base path not found")

        return try {
            LOG.info("Running: ${command.joinToString(" ")} in $basePath")

            val process = ProcessBuilder(command)
                .directory(basePath)
                .redirectErrorStream(true)
                .start()

            val output = StringBuilder()
            BufferedReader(InputStreamReader(process.inputStream)).use { reader ->
                var line: String?
                while (reader.readLine().also { line = it } != null) {
                    output.appendLine(line)
                    LOG.info("bleep: $line")
                    indicator.text2 = line
                    if (indicator.isCanceled) {
                        process.destroyForcibly()
                        return CommandResult(false, output.toString(), "Command cancelled")
                    }
                }
            }

            val exitCode = process.waitFor()
            if (exitCode != 0) {
                LOG.warn("Bleep command failed with exit code $exitCode:\n$output")
                // Extract last few lines as error message
                val errorLines = output.toString().lines().takeLast(5).joinToString("\n")
                CommandResult(false, output.toString(), "Exit code $exitCode: $errorLines")
            } else {
                LOG.info("Bleep command succeeded")
                CommandResult(true, output.toString(), null)
            }
        } catch (e: Exception) {
            LOG.error("Failed to run bleep command: ${e.message}", e)
            CommandResult(false, "", "Exception: ${e.message}")
        }
    }

    /**
     * Trigger a BSP sync after bleep.yaml changes.
     * This reloads the config, notifies listeners, and refreshes the BSP project.
     */
    fun triggerBspSync() {
        LOG.info("Triggering BSP sync (bleep.yaml changed)")
        reloadConfig()
        notifyConfigChanged()
        refreshBsp { success, errorMessage ->
            if (success) {
                LOG.info("BSP sync completed successfully")
            } else {
                LOG.warn("BSP sync failed: $errorMessage")
            }
        }
    }

    /**
     * Refresh BSP project to import the .bsp/bleep.json configuration.
     * Callback receives (success, errorMessage).
     */
    fun refreshBsp(onComplete: (Boolean, String?) -> Unit) {
        val basePath = projectBasePath ?: run {
            onComplete(false, "Project base path not found")
            return
        }

        // First refresh VFS to detect the new .bsp file
        val bspDir = File(basePath, ".bsp")
        val bspFile = File(bspDir, "bleep.json")

        LOG.info("Checking for BSP file at $bspFile (exists: ${bspFile.exists()})")

        if (!bspFile.exists()) {
            LOG.warn("BSP file not found at $bspFile")
            bspStatus = BspStatus.ERROR
            onComplete(false, "BSP file not found at $bspFile - run setup-ide first")
            return
        }

        // Refresh VFS to pick up the new file
        LocalFileSystem.getInstance().refreshAndFindFileByIoFile(bspFile)
        VfsUtil.markDirtyAndRefresh(false, true, true, bspDir)

        LOG.info("Triggering BSP sync for project at $basePath")

        // Try to invoke BSP refresh action
        @Suppress("DEPRECATION")
        try {
            val actionManager = ActionManager.getInstance()

            // Try different possible BSP action IDs
            val actionIds = listOf(
                "bsp.ReloadAllProjects",
                "bsp.ReloadProject",
                "ExternalSystem.RefreshAllProjects",
                "BSP.ReimportProject"
            )

            var actionInvoked = false
            for (actionId in actionIds) {
                val action = actionManager.getAction(actionId)
                if (action != null) {
                    LOG.info("Found BSP action: $actionId")
                    val dataContext = DataContext { dataId ->
                        when (dataId) {
                            "project" -> project
                            else -> null
                        }
                    }
                    val event = AnActionEvent.createFromAnAction(action, null, "", dataContext)
                    action.actionPerformed(event)
                    actionInvoked = true
                    bspStatus = BspStatus.CONNECTED
                    onComplete(true, null)
                    break
                }
            }

            if (!actionInvoked) {
                LOG.info("No BSP action found, falling back to ExternalSystemUtil")
                // Fallback to external system refresh
                val importSpec = ImportSpecBuilder(project, BSP_SYSTEM_ID)
                    .use(ProgressExecutionMode.IN_BACKGROUND_ASYNC)
                    .callback(object : ExternalProjectRefreshCallback {
                        override fun onSuccess(externalProject: com.intellij.openapi.externalSystem.model.DataNode<com.intellij.openapi.externalSystem.model.project.ProjectData>?) {
                            LOG.info("BSP project refresh succeeded")
                            bspStatus = BspStatus.CONNECTED
                            onComplete(true, null)
                        }

                        override fun onFailure(errorMessage: String, errorDetails: String?) {
                            LOG.warn("BSP project refresh failed: $errorMessage\nDetails: $errorDetails")
                            bspStatus = BspStatus.ERROR
                            val fullError = if (errorDetails != null) "$errorMessage: $errorDetails" else errorMessage
                            onComplete(false, "BSP import failed: $fullError")
                        }
                    })
                    .build()

                ExternalSystemUtil.refreshProject(basePath.absolutePath, importSpec)
            }
        } catch (e: Exception) {
            LOG.error("Failed to refresh BSP project", e)
            onComplete(false, "BSP refresh exception: ${e.message}")
        }
    }

    /**
     * Run a bleep script using IntelliJ's run configuration system.
     */
    fun runBleepScript(script: ScriptInfo, debug: Boolean, onComplete: (Boolean) -> Unit) {
        com.intellij.openapi.application.ApplicationManager.getApplication().invokeLater {
            try {
                runMainClass(
                    moduleName = script.project,
                    mainClass = script.mainClass,
                    programArguments = "",
                    runName = "Script: ${script.name}",
                    debug = debug
                )
                onComplete(true)
            } catch (e: Exception) {
                LOG.error("Failed to run script ${script.name}", e)
                onComplete(false)
            }
        }
    }

    /**
     * Run a bleep source generator using IntelliJ's run configuration system.
     */
    fun runBleepSourceGen(sourceGen: SourceGenInfo, targetProjects: List<String>, debug: Boolean, onComplete: (Boolean) -> Unit) {
        com.intellij.openapi.application.ApplicationManager.getApplication().invokeLater {
            try {
                // Build -p arguments for each target project
                val programArgs = targetProjects.flatMap { listOf("-p", it) }.joinToString(" ")

                runMainClass(
                    moduleName = sourceGen.sourceGenProject,
                    mainClass = sourceGen.mainClass,
                    programArguments = programArgs,
                    runName = "SourceGen: ${sourceGen.mainClass.substringAfterLast('.')}",
                    debug = debug
                )
                onComplete(true)
            } catch (e: Exception) {
                LOG.error("Failed to run source generator", e)
                onComplete(false)
            }
        }
    }

    /**
     * Run a main class using IntelliJ's Application run configuration.
     */
    private fun runMainClass(moduleName: String, mainClass: String, programArguments: String, runName: String, debug: Boolean) {
        val mode = if (debug) "Debugging" else "Running"
        LOG.info("$mode $mainClass from module $moduleName with args: $programArguments")

        val runManager = com.intellij.execution.RunManager.getInstance(project)

        // Find the module
        val moduleManager = com.intellij.openapi.module.ModuleManager.getInstance(project)
        val module = moduleManager.modules.find { it.name == moduleName || it.name.endsWith(".$moduleName") }

        if (module == null) {
            LOG.warn("Module not found: $moduleName. Available modules: ${moduleManager.modules.map { it.name }}")
            // Try to run anyway with first module or no module
        }

        // Look for existing Application configuration type
        val configurationType = com.intellij.execution.configurations.ConfigurationTypeUtil
            .findConfigurationType("Application")

        if (configurationType == null) {
            LOG.error("Application configuration type not found")
            return
        }

        // Create a new run configuration
        val configurationFactory = configurationType.configurationFactories.firstOrNull()
        if (configurationFactory == null) {
            LOG.error("No configuration factory found for Application type")
            return
        }

        val settings = runManager.createConfiguration(runName, configurationFactory)
        val configuration = settings.configuration

        // Configure the Application run configuration using reflection to be compatible
        try {
            // Set main class
            val setMainClassMethod = configuration.javaClass.getMethod("setMainClassName", String::class.java)
            setMainClassMethod.invoke(configuration, mainClass)

            // Set module if found
            if (module != null) {
                val setModuleMethod = configuration.javaClass.getMethod("setModule", com.intellij.openapi.module.Module::class.java)
                setModuleMethod.invoke(configuration, module)
            }

            // Set program parameters
            if (programArguments.isNotEmpty()) {
                val setProgramParametersMethod = configuration.javaClass.getMethod("setProgramParameters", String::class.java)
                setProgramParametersMethod.invoke(configuration, programArguments)
            }

            // Set working directory
            projectBasePath?.let { basePath ->
                val setWorkingDirectoryMethod = configuration.javaClass.getMethod("setWorkingDirectory", String::class.java)
                setWorkingDirectoryMethod.invoke(configuration, basePath.absolutePath)
            }
        } catch (e: Exception) {
            LOG.warn("Failed to configure run configuration via reflection", e)
        }

        // Make it temporary (not saved)
        settings.isTemporary = true

        // Add and select the configuration
        runManager.addConfiguration(settings)
        runManager.selectedConfiguration = settings

        // Execute with either Run or Debug executor
        val executor = if (debug) {
            com.intellij.execution.executors.DefaultDebugExecutor.getDebugExecutorInstance()
        } else {
            com.intellij.execution.executors.DefaultRunExecutor.getRunExecutorInstance()
        }
        com.intellij.execution.ProgramRunnerUtil.executeConfiguration(settings, executor)
    }

    override fun dispose() {
        // Cleanup if needed
    }
}
