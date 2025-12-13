package build.bleep.intellij

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.progress.ProgressIndicator
import com.intellij.openapi.progress.ProgressManager
import com.intellij.openapi.progress.Task
import com.intellij.openapi.project.DumbAware
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.SimpleToolWindowPanel
import com.intellij.openapi.wm.ToolWindow
import com.intellij.openapi.wm.ToolWindowFactory
import com.intellij.icons.AllIcons
import com.intellij.ui.CheckboxTree
import com.intellij.ui.CheckedTreeNode
import com.intellij.ui.JBColor
import com.intellij.ui.components.JBLabel
import com.intellij.ui.components.JBScrollPane
import com.intellij.ui.components.JBTabbedPane
import com.intellij.ui.content.ContentFactory
import com.intellij.openapi.Disposable
import com.intellij.openapi.util.Disposer
import com.intellij.execution.executors.DefaultRunExecutor
import com.intellij.execution.runners.ExecutionEnvironmentBuilder
import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.execution.process.ProcessHandlerFactory
import com.intellij.terminal.TerminalExecutionConsole
import com.intellij.openapi.wm.ToolWindowManager
import com.intellij.execution.ui.RunContentDescriptor
import com.intellij.execution.ui.RunContentManager
import java.awt.BorderLayout
import java.awt.Cursor
import java.awt.Dimension
import java.awt.FlowLayout
import java.awt.GridBagConstraints
import java.awt.GridBagLayout
import javax.swing.*
import javax.swing.border.TitledBorder
import javax.swing.tree.DefaultTreeModel

/**
 * Tool window factory for the Bleep panel.
 */
class BleepToolWindowFactory : ToolWindowFactory, DumbAware {
    private val LOG = Logger.getInstance(BleepToolWindowFactory::class.java)

    @Suppress("OVERRIDE_DEPRECATION")
    override fun isApplicable(project: Project): Boolean {
        return BleepConfig.isBleepProject(project.basePath?.let { java.io.File(it) } ?: return false)
    }

    override fun createToolWindowContent(project: Project, toolWindow: ToolWindow) {
        val panel = BleepToolWindowPanel(project)
        val content = ContentFactory.getInstance().createContent(panel, "", false)
        toolWindow.contentManager.addContent(content)
    }
}

/**
 * Panel state machine states.
 */
enum class PanelState {
    INITIALIZING,  // Reading bleep.yaml
    LOADING,       // Loading projects/scripts from bleep CLI
    READY,         // Loaded, no pending changes
    DIRTY,         // User changed selection, not yet applied
    APPLYING,      // Running bleep setup-ide
    ERROR          // Something failed
}

/**
 * Main panel for the Bleep tool window.
 */
class BleepToolWindowPanel(private val project: Project) : SimpleToolWindowPanel(true, true), Disposable {
    private val LOG = Logger.getInstance(BleepToolWindowPanel::class.java)
    private val service = BleepService.getInstance(project)

    // State machine
    private var state: PanelState = PanelState.INITIALIZING
        set(value) {
            field = value
            updateUIForState()
        }

    // UI components
    private val statusLabel = JBLabel("Initializing...")
    private val versionLabel = JBLabel("")
    private val bspStatusLabel = JBLabel("BSP: checking...")
    private val errorPanel = JPanel(BorderLayout()).apply {
        isVisible = false
        background = JBColor(0xFFE0E0, 0x5C3030)
        border = BorderFactory.createCompoundBorder(
            BorderFactory.createMatteBorder(0, 0, 1, 0, JBColor.RED),
            BorderFactory.createEmptyBorder(5, 10, 5, 10)
        )
    }
    private val errorLabel = JBLabel().apply {
        foreground = JBColor(0xCC0000, 0xFF6B6B)
    }
    private val errorDismissButton = JButton("X").apply {
        isBorderPainted = false
        isContentAreaFilled = false
        toolTipText = "Dismiss"
        addActionListener { clearError() }
    }

    // Timer for polling BSP status
    private val bspPollTimer = Timer(2000) { checkBspStatus() }
    private val groupsPanel = JPanel(FlowLayout(FlowLayout.LEFT, 5, 2))
    private val scriptsPanel = JPanel(FlowLayout(FlowLayout.LEFT, 5, 2))
    private val sourceGenPanel = JPanel(FlowLayout(FlowLayout.LEFT, 5, 2))
    private val projectTree: CheckboxTree
    private val rootNode = CheckedTreeNode("Projects")
    private lateinit var treeWrapper: JPanel
    private val setupButton = JButton(AllIcons.Actions.Execute).apply {
        toolTipText = "Setup IDE / Reload BSP"
    }
    private val stopBspButton = JButton(AllIcons.Actions.Suspend).apply {
        toolTipText = "Stop BSP Server"
    }

    private var allProjects: List<ProjectInfo> = emptyList()
    private var groups: List<GroupInfo> = emptyList()
    private var scripts: List<ScriptInfo> = emptyList()
    private var sourceGens: List<SourceGenInfo> = emptyList()

    // Maps for dependency graph traversal
    private var projectByName: Map<String, ProjectInfo> = emptyMap()
    private var dependents: Map<String, Set<String>> = emptyMap() // project -> projects that depend on it
    private var nodeByName: Map<String, CheckedTreeNode> = emptyMap()

    // Track applied selection to detect dirty state
    private var appliedSelection: Set<String> = emptySet()

    // Current error message (if any)
    private var currentError: String? = null

    // Flag to prevent recursive updates
    private var updatingTree = false

    // Listener for config changes (bleep.yaml modified)
    private val configChangeListener: () -> Unit = {
        ApplicationManager.getApplication().invokeLater {
            LOG.info("bleep.yaml changed, refreshing projects")
            refreshProjects()
        }
    }

    init {
        @Suppress("DEPRECATION")
        projectTree = object : CheckboxTree(ProjectTreeCellRenderer(), rootNode) {
            override fun onNodeStateChanged(node: CheckedTreeNode?) {
                if (updatingTree) return
                if (node == null) return

                val projectName = node.userObject as? String ?: return

                // Prevent CheckboxTree from propagating to children - only update the single clicked project
                updatingTree = true
                try {
                    if (node.isChecked) {
                        // When selecting: select this project + its dependencies (things it needs)
                        val toSelect = mutableSetOf<String>()
                        collectDependencies(projectName, toSelect)
                        toSelect.forEach { name ->
                            service.setSelectedProjects(service.getSelectedProjects() + name)
                            nodeByName[name]?.isChecked = true
                        }
                    } else {
                        // When deselecting: only deselect this single project
                        service.setSelectedProjects(service.getSelectedProjects() - projectName)
                        node.isChecked = false
                    }
                    (projectTree.model as DefaultTreeModel).nodeChanged(rootNode)
                } finally {
                    updatingTree = false
                }
                updateCountLabel()
            }
        }

        // === Error panel setup ===
        errorPanel.add(errorLabel, BorderLayout.CENTER)
        errorPanel.add(errorDismissButton, BorderLayout.EAST)

        // === Header panel (error + status + buttons at top) ===
        val headerPanel = JPanel().apply {
            layout = BoxLayout(this, BoxLayout.Y_AXIS)

            // Error row (hidden by default)
            errorPanel.alignmentX = java.awt.Component.LEFT_ALIGNMENT
            add(errorPanel)

            // Status row
            val statusRow = JPanel(FlowLayout(FlowLayout.LEFT, 5, 2)).apply {
                alignmentX = java.awt.Component.LEFT_ALIGNMENT
                add(versionLabel)
                add(Box.createHorizontalStrut(10))
                add(bspStatusLabel)
                add(Box.createHorizontalStrut(10))
                add(statusLabel)
            }
            add(statusRow)

            // Button row
            val buttonRow = JPanel(FlowLayout(FlowLayout.LEFT, 5, 2)).apply {
                alignmentX = java.awt.Component.LEFT_ALIGNMENT
                add(setupButton)
                add(stopBspButton)
            }
            add(buttonRow)
        }

        // === TAB 1: Projects ===
        val projectsTab = JPanel(BorderLayout()).apply {
            // Groups section (compact)
            val groupsWrapper = JPanel(BorderLayout()).apply {
                border = BorderFactory.createTitledBorder("Groups")
                add(JBScrollPane(groupsPanel).apply {
                    preferredSize = java.awt.Dimension(0, 40)
                    minimumSize = java.awt.Dimension(0, 40)
                }, BorderLayout.CENTER)
            }

            // Project tree
            treeWrapper = JPanel(BorderLayout()).apply {
                border = BorderFactory.createTitledBorder("Projects (0 / 0)")
                add(JBScrollPane(projectTree), BorderLayout.CENTER)
            }

            add(groupsWrapper, BorderLayout.NORTH)
            add(treeWrapper, BorderLayout.CENTER)
        }

        // === TAB 2: Scripts & Source Generators (single scrollable list) ===
        val scriptsTab = JPanel(BorderLayout()).apply {
            val contentPanel = JPanel().apply {
                layout = BoxLayout(this, BoxLayout.Y_AXIS)

                // Scripts header
                add(JBLabel("Scripts").apply {
                    font = font.deriveFont(java.awt.Font.BOLD)
                    border = BorderFactory.createEmptyBorder(5, 5, 5, 5)
                    alignmentX = java.awt.Component.LEFT_ALIGNMENT
                })
                scriptsPanel.alignmentX = java.awt.Component.LEFT_ALIGNMENT
                add(scriptsPanel)

                add(Box.createVerticalStrut(15))

                // Source Generators header
                add(JBLabel("Source Generators").apply {
                    font = font.deriveFont(java.awt.Font.BOLD)
                    border = BorderFactory.createEmptyBorder(5, 5, 5, 5)
                    alignmentX = java.awt.Component.LEFT_ALIGNMENT
                })
                sourceGenPanel.alignmentX = java.awt.Component.LEFT_ALIGNMENT
                add(sourceGenPanel)

                // Push content to top
                add(Box.createVerticalGlue())
            }

            add(JBScrollPane(contentPanel), BorderLayout.CENTER)
        }

        // === TAB 3: Actions (build commands) ===
        val actionsTab = JPanel(BorderLayout()).apply {
            val actionsPanel = JPanel().apply {
                layout = BoxLayout(this, BoxLayout.Y_AXIS)
                border = BorderFactory.createEmptyBorder(5, 5, 5, 5)

                // normalize - no args
                add(createCommandPanel(
                    command = "normalize",
                    description = "Normalize build (deduplicate, sort, etc.)",
                    inputsBuilder = null
                ))

                // templates-reapply - no args
                add(createCommandPanel(
                    command = "templates-reapply",
                    description = "Apply existing templates again",
                    inputsBuilder = null
                ))

                // templates-generate-new - no args
                add(createCommandPanel(
                    command = "templates-generate-new",
                    description = "Throw away existing templates and infer new",
                    inputsBuilder = null
                ))

                // move-files-into-bleep-layout - no args
                add(createCommandPanel(
                    command = "move-files-into-bleep-layout",
                    description = "Move source files from sbt layout to bleep layout",
                    inputsBuilder = null
                ))

                // create-directories - optional cross-project list
                add(createCommandPanel(
                    command = "create-directories",
                    description = "Create all source and resource folders for project(s)",
                    inputsBuilder = { panel, argsCollector ->
                        addProjectMultiSelect(panel, argsCollector, "Cross-projects (optional):", false, ProjectNameType.CROSS_PROJECT_NAME)
                    }
                ))

                // update-deps - optional flags
                add(createCommandPanel(
                    command = "update-deps",
                    description = "Update to newest versions of all dependencies",
                    inputsBuilder = { panel, argsCollector ->
                        addUpdateDepsFlags(panel, argsCollector)
                    }
                ))

                // update-dep - required dep + optional flags
                add(createCommandPanel(
                    command = "update-dep",
                    description = "Update a single dependency or organization to newest version",
                    inputsBuilder = { panel, argsCollector ->
                        addTextField(panel, argsCollector, "Dependency (org:module or org):", "dependency", true)
                        addUpdateDepsFlags(panel, argsCollector)
                    }
                ))

                // project-rename - from project name, to name (uses ProjectName, not cross)
                add(createCommandPanel(
                    command = "project-rename",
                    description = "Rename a project",
                    inputsBuilder = { panel, argsCollector ->
                        addProjectDropdown(panel, argsCollector, "Project to rename:", "from", true, ProjectNameType.PROJECT_NAME)
                        addTextField(panel, argsCollector, "New name:", "to", true)
                    }
                ))

                // project-merge-into - from project name, into project name (uses ProjectName, not cross)
                add(createCommandPanel(
                    command = "project-merge-into",
                    description = "Merge first project into second",
                    inputsBuilder = { panel, argsCollector ->
                        addProjectDropdown(panel, argsCollector, "Merge from:", "from", true, ProjectNameType.PROJECT_NAME)
                        addProjectDropdown(panel, argsCollector, "Into:", "into", true, ProjectNameType.PROJECT_NAME)
                    }
                ))

                // projects-move - folder + project names (uses ProjectName, not cross)
                add(createCommandPanel(
                    command = "projects-move",
                    description = "Move projects to a new parent folder",
                    inputsBuilder = { panel, argsCollector ->
                        addTextField(panel, argsCollector, "New parent folder:", "folder", true)
                        addProjectMultiSelect(panel, argsCollector, "Projects to move:", true, ProjectNameType.PROJECT_NAME)
                    }
                ))

                // diff exploded - optional cross-projects + revision
                add(createCommandPanel(
                    command = "diff exploded",
                    description = "Diff exploded projects compared to git HEAD or revision",
                    inputsBuilder = { panel, argsCollector ->
                        addTextField(panel, argsCollector, "Git revision (optional, default HEAD):", "revision", false, "-r")
                        addProjectMultiSelect(panel, argsCollector, "Cross-projects (optional, default all):", false, ProjectNameType.CROSS_PROJECT_NAME)
                    }
                ))

                // diff bloop - optional cross-projects + revision
                add(createCommandPanel(
                    command = "diff bloop",
                    description = "Diff bloop projects compared to git HEAD or revision",
                    inputsBuilder = { panel, argsCollector ->
                        addTextField(panel, argsCollector, "Git revision (optional, default HEAD):", "revision", false, "-r")
                        addProjectMultiSelect(panel, argsCollector, "Cross-projects (optional, default all):", false, ProjectNameType.CROSS_PROJECT_NAME)
                    }
                ))

                // show short - required project names (uses ProjectName, shows YAML definition)
                add(createCommandPanel(
                    command = "show short",
                    description = "Show projects as written in YAML",
                    inputsBuilder = { panel, argsCollector ->
                        addProjectMultiSelect(panel, argsCollector, "Projects:", true, ProjectNameType.PROJECT_NAME)
                    }
                ))

                // show exploded - optional cross-projects
                add(createCommandPanel(
                    command = "show exploded",
                    description = "Show projects after templates have been applied",
                    inputsBuilder = { panel, argsCollector ->
                        addProjectMultiSelect(panel, argsCollector, "Cross-projects (optional, default all):", false, ProjectNameType.CROSS_PROJECT_NAME)
                    }
                ))

                // show bloop - optional cross-projects
                add(createCommandPanel(
                    command = "show bloop",
                    description = "Show projects as they appear to bloop with absolute paths",
                    inputsBuilder = { panel, argsCollector ->
                        addProjectMultiSelect(panel, argsCollector, "Cross-projects (optional, default all):", false, ProjectNameType.CROSS_PROJECT_NAME)
                    }
                ))

                add(Box.createVerticalGlue())
            }

            add(JBScrollPane(actionsPanel), BorderLayout.CENTER)
        }

        // === Tabbed pane ===
        val tabbedPane = JBTabbedPane().apply {
            addTab("Project Picker", projectsTab)
            addTab("Run", scriptsTab)
            addTab("Build Actions", actionsTab)
        }

        // === Main layout ===
        setContent(JPanel(BorderLayout()).apply {
            add(headerPanel, BorderLayout.NORTH)
            add(tabbedPane, BorderLayout.CENTER)
        })

        setupButton.addActionListener { onSetupIde() }
        stopBspButton.addActionListener { onStopBsp() }

        // Start BSP status polling
        bspPollTimer.isRepeats = true
        bspPollTimer.start()

        // Register for disposal
        Disposer.register(service, this)

        // Subscribe to config changes (bleep.yaml modifications)
        service.addConfigChangeListener(configChangeListener)

        // Initial load - state machine starts at INITIALIZING
        state = PanelState.INITIALIZING
        refreshProjects()
        checkBspStatus()
    }

    override fun dispose() {
        bspPollTimer.stop()
        service.removeConfigChangeListener(configChangeListener)
    }

    /**
     * Update UI based on current state.
     */
    private fun updateUIForState() {
        ApplicationManager.getApplication().invokeLater {
            when (state) {
                PanelState.INITIALIZING -> {
                    statusLabel.text = "Initializing..."
                    statusLabel.foreground = JBColor.GRAY
                    setupButton.isEnabled = false
                    stopBspButton.isEnabled = false
                    projectTree.isEnabled = false
                }
                PanelState.LOADING -> {
                    statusLabel.text = "Loading projects..."
                    statusLabel.foreground = JBColor.GRAY
                    setupButton.isEnabled = false
                    stopBspButton.isEnabled = true
                    projectTree.isEnabled = false
                }
                PanelState.READY -> {
                    statusLabel.text = "Ready"
                    statusLabel.foreground = JBColor.GREEN
                    setupButton.isEnabled = true
                    setupButton.toolTipText = "Reload BSP"
                    stopBspButton.isEnabled = true
                    projectTree.isEnabled = true
                }
                PanelState.DIRTY -> {
                    statusLabel.text = "Changes pending - click play to apply"
                    statusLabel.foreground = JBColor.ORANGE
                    setupButton.isEnabled = true
                    setupButton.toolTipText = "Apply changes and reload BSP"
                    stopBspButton.isEnabled = true
                    projectTree.isEnabled = true
                }
                PanelState.APPLYING -> {
                    statusLabel.text = "Setting up IDE..."
                    statusLabel.foreground = JBColor.GRAY
                    setupButton.isEnabled = false
                    stopBspButton.isEnabled = false
                    projectTree.isEnabled = false
                }
                PanelState.ERROR -> {
                    statusLabel.text = "Error"
                    statusLabel.foreground = JBColor.RED
                    setupButton.isEnabled = true
                    setupButton.toolTipText = "Retry setup"
                    stopBspButton.isEnabled = true
                    projectTree.isEnabled = true
                }
            }
        }
    }

    /**
     * Show an error message in the error panel.
     */
    private fun showError(message: String) {
        currentError = message
        LOG.warn("Bleep error: $message")
        ApplicationManager.getApplication().invokeLater {
            errorLabel.text = message
            errorPanel.isVisible = true
            errorPanel.revalidate()
            errorPanel.repaint()
            state = PanelState.ERROR
        }
    }

    /**
     * Clear the error panel.
     */
    private fun clearError() {
        currentError = null
        ApplicationManager.getApplication().invokeLater {
            errorPanel.isVisible = false
            errorPanel.revalidate()
            // Return to appropriate state
            state = if (service.getSelectedProjects() != appliedSelection) {
                PanelState.DIRTY
            } else {
                PanelState.READY
            }
        }
    }

    /**
     * Check if current selection differs from applied selection and update state.
     */
    private fun checkDirtyState() {
        if (state == PanelState.APPLYING || state == PanelState.LOADING || state == PanelState.INITIALIZING) {
            return // Don't change state during operations
        }
        val isDirty = service.getSelectedProjects() != appliedSelection
        if (isDirty && state != PanelState.DIRTY && state != PanelState.ERROR) {
            state = PanelState.DIRTY
        } else if (!isDirty && state == PanelState.DIRTY) {
            state = PanelState.READY
        }
    }

    /**
     * Check if BSP server is running by querying BspCommunicationService.
     */
    private fun checkBspStatus() {
        try {
            val serviceClass = Class.forName("org.jetbrains.bsp.protocol.BspCommunicationService")
            val getInstanceMethod = serviceClass.getMethod("getInstance")
            val serviceInstance = getInstanceMethod.invoke(null)

            // Get the internal comms TrieMap to check alive status
            val commsField = serviceClass.getDeclaredField("org\$jetbrains\$bsp\$protocol\$BspCommunicationService\$\$comms")
            commsField.isAccessible = true
            val commsMap = commsField.get(serviceInstance)

            // Get values from the TrieMap
            val valuesMethod = commsMap.javaClass.getMethod("values")
            val values = valuesMethod.invoke(commsMap)

            // Convert to iterator and count alive connections
            val iteratorMethod = values.javaClass.getMethod("iterator")
            val iterator = iteratorMethod.invoke(values)
            val hasNextMethod = iterator.javaClass.getMethod("hasNext")
            val nextMethod = iterator.javaClass.getMethod("next")

            var aliveCount = 0
            while (hasNextMethod.invoke(iterator) as Boolean) {
                val comm = nextMethod.invoke(iterator)
                val aliveMethod = comm.javaClass.getMethod("alive")
                val isAlive = aliveMethod.invoke(comm) as Boolean
                if (isAlive) aliveCount++
            }

            ApplicationManager.getApplication().invokeLater {
                if (aliveCount == 0) {
                    bspStatusLabel.text = "BSP: not running"
                    bspStatusLabel.foreground = JBColor.GRAY
                    service.bspStatus = BleepService.BspStatus.NOT_CONFIGURED
                } else {
                    bspStatusLabel.text = "BSP: $aliveCount alive"
                    bspStatusLabel.foreground = JBColor.GREEN
                    service.bspStatus = BleepService.BspStatus.CONNECTED
                }
            }
        } catch (e: ClassNotFoundException) {
            ApplicationManager.getApplication().invokeLater {
                bspStatusLabel.text = "BSP: n/a"
                bspStatusLabel.foreground = JBColor.GRAY
            }
        } catch (e: Exception) {
            LOG.warn("Failed to check BSP status: ${e.message}", e)
            ApplicationManager.getApplication().invokeLater {
                bspStatusLabel.text = "BSP: error"
                bspStatusLabel.foreground = JBColor.RED
            }
        }
    }

    /**
     * Select a project and all its dependencies (transitively).
     */
    private fun selectWithDependencies(projectName: String) {
        val toSelect = mutableSetOf<String>()
        collectDependencies(projectName, toSelect)

        toSelect.forEach { name ->
            service.setSelectedProjects(service.getSelectedProjects() + name)
        }

        // Update tree checkboxes
        updatingTree = true
        toSelect.forEach { name ->
            nodeByName[name]?.isChecked = true
        }
        updatingTree = false
        (projectTree.model as DefaultTreeModel).nodeChanged(rootNode)
    }

    /**
     * Collect all dependencies of a project (transitively).
     */
    private fun collectDependencies(projectName: String, result: MutableSet<String>) {
        if (projectName in result) return
        result.add(projectName)

        projectByName[projectName]?.dependsOn?.forEach { dep ->
            collectDependencies(dep, result)
        }
    }

    /**
     * Deselect a single project without cascading.
     * This allows users to create "invalid" selections which they can fix themselves,
     * rather than having aggressive auto-deselection that's hard to predict.
     */
    private fun deselectSingleProject(projectName: String) {
        service.setSelectedProjects(service.getSelectedProjects() - projectName)

        // Update tree checkbox
        updatingTree = true
        nodeByName[projectName]?.isChecked = false
        updatingTree = false
        (projectTree.model as DefaultTreeModel).nodeChanged(rootNode)
    }

    private fun updateCountLabel() {
        val selected = service.getSelectedProjects().size
        val total = allProjects.size
        treeWrapper.border = BorderFactory.createTitledBorder("Projects ($selected / $total)")
        checkDirtyState()
    }

    private fun refreshProjects() {
        state = PanelState.LOADING
        rootNode.removeAllChildren()
        groupsPanel.removeAll()
        scriptsPanel.removeAll()
        sourceGenPanel.removeAll()

        ProgressManager.getInstance().run(object : Task.Backgroundable(project, "Loading Bleep Projects", false) {
            override fun run(indicator: ProgressIndicator) {
                try {
                    val graph = service.getProjectGraph(indicator)
                    val groupsData = service.getProjectGroups(indicator)
                    val scriptsData = service.getScripts(indicator)
                    val sourceGenData = service.getSourceGen(indicator)

                    // Load saved selection from file
                    val savedSelection = service.loadProjectSelection()
                    if (savedSelection.isNotEmpty()) {
                        service.setSelectedProjects(savedSelection)
                    }
                    val selected = service.getSelectedProjects()

                    ApplicationManager.getApplication().invokeLater {
                        if (graph == null) {
                            showError("Failed to load project graph from bleep CLI")
                            return@invokeLater
                        }

                        allProjects = graph.projects
                        buildDependencyMaps()
                        populateProjectTree(selected)

                        if (groupsData != null) {
                            groups = groupsData.groups.filter { !isCrossProjectGroup(it) }
                            populateGroups()
                        }

                        if (scriptsData != null) {
                            scripts = scriptsData.scripts
                            populateScripts()
                        }

                        if (sourceGenData != null) {
                            sourceGens = sourceGenData.sourcegens
                            populateSourceGens()
                        }

                        // Set initial applied selection
                        appliedSelection = selected
                        updateCountLabel()

                        val config = service.bleepConfig
                        if (config != null) {
                            versionLabel.text = "Bleep ${config.version}"
                        }

                        state = PanelState.READY
                    }
                } catch (e: Exception) {
                    LOG.error("Failed to load projects", e)
                    ApplicationManager.getApplication().invokeLater {
                        showError("Failed to load projects: ${e.message}")
                    }
                }
            }
        })
    }

    /**
     * Build maps for dependency graph traversal.
     */
    private fun buildDependencyMaps() {
        projectByName = allProjects.associateBy { it.name }

        // Build reverse dependency map (who depends on whom)
        val deps = mutableMapOf<String, MutableSet<String>>()
        allProjects.forEach { project ->
            project.dependsOn.forEach { dependency ->
                deps.getOrPut(dependency) { mutableSetOf() }.add(project.name)
            }
        }
        dependents = deps
    }

    /**
     * Check if a group is just cross-compilation variants of the same project.
     * E.g., a group containing only "mylib@jvm213", "mylib@jvm3", "mylib@js3"
     */
    private fun isCrossProjectGroup(group: GroupInfo): Boolean {
        if (group.projects.size <= 1) return true

        // Extract base names (part before @)
        val baseNames = group.projects.map { name ->
            val atIndex = name.indexOf('@')
            if (atIndex > 0) name.substring(0, atIndex) else name
        }.toSet()

        // If all projects have the same base name, it's a cross-project group
        return baseNames.size == 1
    }

    /**
     * Populate the project tree with a best-effort tree-like structure.
     * Projects are shown in a hierarchical way where children appear under
     * the parent that "unlocks" them (i.e., the last dependency to be satisfied).
     */
    private fun populateProjectTree(selected: Set<String>) {
        rootNode.removeAllChildren()
        val nodeMap = mutableMapOf<String, CheckedTreeNode>()
        val listed = mutableSetOf<String>()
        val projectNames = allProjects.map { it.name }.toSet()

        // Count downstream dependents for sorting
        val downstreamCount = allProjects.associate { project ->
            project.name to (dependents[project.name]?.size ?: 0)
        }

        // Get internal dependencies for a project
        fun getInternalDeps(name: String): Set<String> {
            val project = projectByName[name] ?: return emptySet()
            return project.dependsOn.filter { it in projectNames }.toSet()
        }

        // Check if all dependencies of a project are satisfied (listed)
        fun allDepsSatisfied(name: String): Boolean {
            return getInternalDeps(name).all { it in listed }
        }

        // Recursively add a project and any dependents that become unlocked
        fun addProjectWithChildren(projectName: String, parentNode: CheckedTreeNode, depth: Int) {
            if (projectName in listed) return

            val node = CheckedTreeNode(projectName)
            node.isChecked = projectName in selected
            nodeMap[projectName] = node
            parentNode.add(node)
            listed.add(projectName)

            // Find dependents that are now unlocked (all their deps are satisfied)
            val myDependents = dependents[projectName] ?: emptySet()
            val unlocked = myDependents
                .filter { it !in listed && allDepsSatisfied(it) }
                .sortedWith(compareByDescending<String> { downstreamCount[it] ?: 0 }.thenBy { it })

            // Add unlocked dependents as children
            unlocked.forEach { dependent ->
                addProjectWithChildren(dependent, node, depth + 1)
            }
        }

        // Start with level 0 projects (no internal dependencies)
        val level0 = allProjects
            .filter { getInternalDeps(it.name).isEmpty() }
            .map { it.name }
            .sortedWith(compareByDescending<String> { downstreamCount[it] ?: 0 }.thenBy { it })

        level0.forEach { projectName ->
            addProjectWithChildren(projectName, rootNode, 0)
        }

        // Add any remaining projects that weren't reached (circular deps or isolated)
        allProjects.map { it.name }
            .filter { it !in listed }
            .sortedWith(compareByDescending<String> { downstreamCount[it] ?: 0 }.thenBy { it })
            .forEach { projectName ->
                val node = CheckedTreeNode(projectName)
                node.isChecked = projectName in selected
                nodeMap[projectName] = node
                rootNode.add(node)
                listed.add(projectName)
            }

        nodeByName = nodeMap
        (projectTree.model as DefaultTreeModel).reload()

        // Expand all nodes
        for (i in 0 until projectTree.rowCount) {
            projectTree.expandRow(i)
        }
    }

    private fun populateGroups() {
        groupsPanel.removeAll()

        // Add "All" group first
        val allGroup = GroupInfo("All", allProjects.map { it.name })
        val allButton = JButton("All (${allProjects.size})").apply {
            toolTipText = "Select/deselect all projects"
            addActionListener {
                toggleGroup(allGroup)
            }
        }
        groupsPanel.add(allButton)

        // Add other groups
        groups.forEach { group ->
            val button = JButton("${group.name} (${group.projects.size})").apply {
                toolTipText = "Projects: ${group.projects.take(5).joinToString(", ")}${if (group.projects.size > 5) "..." else ""}"
                addActionListener {
                    toggleGroup(group)
                }
            }
            groupsPanel.add(button)
        }

        groupsPanel.revalidate()
        groupsPanel.repaint()
    }

    private fun toggleGroup(group: GroupInfo) {
        val selected = service.getSelectedProjects()
        val allInGroup = group.projects.all { it in selected }

        if (allInGroup) {
            // Deselect all in group (only the group members, no cascading)
            group.projects.forEach { projectName ->
                deselectSingleProject(projectName)
            }
        } else {
            // Select all in group (and their dependencies)
            group.projects.forEach { projectName ->
                selectWithDependencies(projectName)
            }
        }

        updateCountLabel()
    }

    private fun populateScripts() {
        scriptsPanel.removeAll()
        scriptsPanel.layout = GridBagLayout()

        val gbc = GridBagConstraints().apply {
            anchor = GridBagConstraints.WEST
            insets = java.awt.Insets(2, 4, 2, 4)
        }

        scripts.sortedBy { it.name }.forEachIndexed { index, script ->
            gbc.gridy = index

            // Name column
            gbc.gridx = 0
            gbc.weightx = 1.0
            gbc.fill = GridBagConstraints.HORIZONTAL
            scriptsPanel.add(JBLabel(script.name).apply {
                toolTipText = "Project: ${script.project}\nMain: ${script.mainClass}"
            }, gbc)

            // Run button
            gbc.gridx = 1
            gbc.weightx = 0.0
            gbc.fill = GridBagConstraints.NONE
            scriptsPanel.add(JButton(AllIcons.Actions.Execute).apply {
                toolTipText = "Run ${script.name}"
                addActionListener { runScript(script, debug = false) }
            }, gbc)

            // Debug button
            gbc.gridx = 2
            scriptsPanel.add(JButton(AllIcons.Actions.StartDebugger).apply {
                toolTipText = "Debug ${script.name}"
                addActionListener { runScript(script, debug = true) }
            }, gbc)
        }

        scriptsPanel.revalidate()
        scriptsPanel.repaint()
    }

    private fun populateSourceGens() {
        sourceGenPanel.removeAll()
        sourceGenPanel.layout = GridBagLayout()

        // Group by sourceGenProject and mainClass to deduplicate
        val uniqueSourceGens = sourceGens
            .groupBy { "${it.sourceGenProject}:${it.mainClass}" }
            .map { (_, gens) -> gens.first() to gens.map { it.project } }
            .sortedBy { it.first.mainClass }

        val gbc = GridBagConstraints().apply {
            anchor = GridBagConstraints.WEST
            insets = java.awt.Insets(2, 4, 2, 4)
        }

        uniqueSourceGens.forEachIndexed { index, (sourceGen, targetProjects) ->
            val displayName = sourceGen.mainClass.substringAfterLast('.')
            gbc.gridy = index

            // Name column
            gbc.gridx = 0
            gbc.weightx = 1.0
            gbc.fill = GridBagConstraints.HORIZONTAL
            sourceGenPanel.add(JBLabel(displayName).apply {
                toolTipText = "Generator: ${sourceGen.mainClass}\nTargets: ${targetProjects.take(5).joinToString(", ")}${if (targetProjects.size > 5) "..." else ""}"
            }, gbc)

            // Run button
            gbc.gridx = 1
            gbc.weightx = 0.0
            gbc.fill = GridBagConstraints.NONE
            sourceGenPanel.add(JButton(AllIcons.Actions.Execute).apply {
                toolTipText = "Run $displayName"
                addActionListener { runSourceGen(sourceGen, targetProjects, debug = false) }
            }, gbc)

            // Debug button
            gbc.gridx = 2
            sourceGenPanel.add(JButton(AllIcons.Actions.StartDebugger).apply {
                toolTipText = "Debug $displayName"
                addActionListener { runSourceGen(sourceGen, targetProjects, debug = true) }
            }, gbc)
        }

        sourceGenPanel.revalidate()
        sourceGenPanel.repaint()
    }

    private fun runScript(script: ScriptInfo, debug: Boolean) {
        val mode = if (debug) "Debugging" else "Running"
        LOG.info("$mode script: ${script.name}")
        service.runBleepScript(script, debug) { success ->
            ApplicationManager.getApplication().invokeLater {
                if (success) {
                    statusLabel.text = "Script '${script.name}' started"
                } else {
                    statusLabel.text = "Script '${script.name}' failed to start"
                }
            }
        }
    }

    private fun runSourceGen(sourceGen: SourceGenInfo, targetProjects: List<String>, debug: Boolean) {
        val mode = if (debug) "Debugging" else "Running"
        LOG.info("$mode source generator: ${sourceGen.mainClass} for ${targetProjects.size} projects")
        service.runBleepSourceGen(sourceGen, targetProjects, debug) { success ->
            ApplicationManager.getApplication().invokeLater {
                if (success) {
                    statusLabel.text = "Source generator started"
                } else {
                    statusLabel.text = "Source generator failed to start"
                }
            }
        }
    }

    private fun selectAll() {
        service.setSelectedProjects(allProjects.map { it.name }.toSet())
        populateProjectTree(service.getSelectedProjects())
        updateCountLabel()
    }

    private fun deselectAll() {
        service.setSelectedProjects(emptySet())
        populateProjectTree(service.getSelectedProjects())
        updateCountLabel()
    }

    private fun onSetupIde() {
        state = PanelState.APPLYING

        service.setupIde { success, errorMessage ->
            ApplicationManager.getApplication().invokeLater {
                if (success) {
                    // Update applied selection to current selection
                    appliedSelection = service.getSelectedProjects()
                    state = PanelState.READY
                } else {
                    showError(errorMessage ?: "Failed to setup IDE")
                }
            }
        }
    }

    private fun onStopBsp() {
        // Use reflection to call BspCommunicationService.getInstance().closeAll()
        // since the Scala plugin classes aren't available at compile time
        try {
            val serviceClass = Class.forName("org.jetbrains.bsp.protocol.BspCommunicationService")
            val getInstanceMethod = serviceClass.getMethod("getInstance")
            val serviceInstance = getInstanceMethod.invoke(null)

            val closeAllMethod = serviceClass.getMethod("closeAll")
            closeAllMethod.invoke(serviceInstance)

            LOG.info("BSP server stopped successfully")
            service.bspStatus = BleepService.BspStatus.NOT_CONFIGURED
            // Trigger immediate BSP status check
            checkBspStatus()
        } catch (e: ClassNotFoundException) {
            showError("BSP service not available - Scala plugin may not be installed")
        } catch (e: Exception) {
            LOG.error("Failed to stop BSP server", e)
            showError("Failed to stop BSP server: ${e.message}")
        }
    }

    // === Actions tab helper methods ===

    /**
     * Whether to use base project names or full cross-project names.
     * ProjectName: Base name like "typo" - for commands that modify project structure
     * CrossProjectName: Full name like "typo@jvm3" - for commands that operate on build targets
     */
    private enum class ProjectNameType {
        PROJECT_NAME,       // Base name without @suffix (e.g., "typo")
        CROSS_PROJECT_NAME  // Full name with @suffix (e.g., "typo@jvm3")
    }

    /**
     * Type alias for collecting command arguments.
     */
    private class ArgsCollector {
        val args = mutableListOf<String>()
        val flags = mutableMapOf<String, () -> Boolean>()
        val values = mutableMapOf<String, () -> String>()
        val flaggedValues = mutableMapOf<String, () -> List<String>>()  // For --flag value pairs
        val multiValues = mutableMapOf<String, () -> List<String>>()
        var required = mutableMapOf<String, () -> Boolean>()

        fun isValid(): Boolean {
            for ((_, checker) in required) {
                if (!checker()) return false
            }
            return true
        }

        fun collectArgs(): List<String> {
            val result = mutableListOf<String>()
            // Add flags
            for ((flag, getter) in flags) {
                if (getter()) result.add(flag)
            }
            // Add flagged values (--flag value pairs)
            for ((_, getter) in flaggedValues) {
                result.addAll(getter())
            }
            // Add single values
            for ((_, getter) in values) {
                val v = getter()
                if (v.isNotBlank()) result.add(v)
            }
            // Add multi values
            for ((_, getter) in multiValues) {
                result.addAll(getter())
            }
            return result
        }
    }

    /**
     * Create an expandable command panel.
     */
    private fun createCommandPanel(
        command: String,
        description: String,
        inputsBuilder: ((JPanel, ArgsCollector) -> Unit)?
    ): JPanel {
        val argsCollector = ArgsCollector()
        val hasInputs = inputsBuilder != null

        return JPanel().apply {
            layout = BorderLayout()
            border = BorderFactory.createCompoundBorder(
                BorderFactory.createMatteBorder(0, 0, 1, 0, JBColor.border()),
                BorderFactory.createEmptyBorder(2, 2, 2, 2)
            )
            alignmentX = java.awt.Component.LEFT_ALIGNMENT
            maximumSize = Dimension(Int.MAX_VALUE, if (hasInputs) Int.MAX_VALUE else 40)

            // Content panel (inputs + run button) - initially hidden if has inputs
            val contentPanel = JPanel().apply {
                layout = BoxLayout(this, BoxLayout.Y_AXIS)
                border = BorderFactory.createEmptyBorder(5, 20, 5, 5)
                isVisible = !hasInputs // Start expanded only for simple commands
            }

            // Build inputs
            if (inputsBuilder != null) {
                inputsBuilder(contentPanel, argsCollector)
            }

            // Run button (in content area for commands with inputs, in header for simple ones)
            val runButton = JButton("Run").apply {
                icon = AllIcons.Actions.Execute
                addActionListener {
                    if (!hasInputs || argsCollector.isValid()) {
                        val args = argsCollector.collectArgs()
                        runBleepInTerminal("build $command", args)
                    }
                }
            }

            if (hasInputs) {
                // Add run button at bottom of content panel
                val buttonPanel = JPanel(FlowLayout(FlowLayout.LEFT)).apply {
                    alignmentX = java.awt.Component.LEFT_ALIGNMENT
                    add(runButton)
                }
                contentPanel.add(Box.createVerticalStrut(5))
                contentPanel.add(buttonPanel)
            }

            // Header panel (clickable to expand/collapse)
            val headerPanel = JPanel(BorderLayout()).apply {
                border = BorderFactory.createEmptyBorder(3, 5, 3, 5)

                val expandIcon = if (hasInputs) {
                    JBLabel(AllIcons.General.ArrowRight).apply {
                        border = BorderFactory.createEmptyBorder(0, 0, 0, 5)
                    }
                } else null

                val titleLabel = JBLabel(command).apply {
                    font = font.deriveFont(java.awt.Font.BOLD)
                }

                val descLabel = JBLabel(" - $description").apply {
                    foreground = JBColor.GRAY
                }

                val leftPanel = JPanel(FlowLayout(FlowLayout.LEFT, 0, 0)).apply {
                    if (expandIcon != null) add(expandIcon)
                    add(titleLabel)
                    add(descLabel)
                }

                add(leftPanel, BorderLayout.CENTER)

                // For simple commands, put run button in header
                if (!hasInputs) {
                    add(runButton, BorderLayout.EAST)
                }

                // Make clickable for commands with inputs
                if (hasInputs) {
                    cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
                    addMouseListener(object : java.awt.event.MouseAdapter() {
                        override fun mouseClicked(e: java.awt.event.MouseEvent) {
                            contentPanel.isVisible = !contentPanel.isVisible
                            expandIcon?.icon = if (contentPanel.isVisible) {
                                AllIcons.General.ArrowDown
                            } else {
                                AllIcons.General.ArrowRight
                            }
                            revalidate()
                            repaint()
                        }
                    })
                }
            }

            add(headerPanel, BorderLayout.NORTH)
            add(contentPanel, BorderLayout.CENTER)
        }
    }

    /**
     * Add a text field input to the panel.
     */
    private fun addTextField(
        panel: JPanel,
        argsCollector: ArgsCollector,
        label: String,
        key: String,
        required: Boolean,
        flagPrefix: String? = null
    ) {
        val textField = JTextField(20)

        val row = JPanel(FlowLayout(FlowLayout.LEFT, 5, 2)).apply {
            alignmentX = java.awt.Component.LEFT_ALIGNMENT
            add(JBLabel(label))
            add(textField)
            if (required) {
                add(JBLabel("*").apply { foreground = JBColor.RED })
            }
        }
        panel.add(row)

        if (flagPrefix != null) {
            // For flagged fields, add both flag and value if non-empty
            argsCollector.flaggedValues[key] = {
                val v = textField.text.trim()
                if (v.isNotBlank()) listOf(flagPrefix, v) else emptyList()
            }
        } else {
            argsCollector.values[key] = { textField.text.trim() }
        }
        if (required) {
            argsCollector.required[key] = { textField.text.trim().isNotBlank() }
        }
    }

    /**
     * Add a project dropdown to the panel.
     * Uses DefaultComboBoxModel so it can be refreshed when projects are loaded.
     * @param nameType Whether to show base project names or full cross-project names
     */
    private fun addProjectDropdown(
        panel: JPanel,
        argsCollector: ArgsCollector,
        label: String,
        key: String,
        required: Boolean,
        nameType: ProjectNameType
    ) {
        val model = javax.swing.DefaultComboBoxModel<String>()
        val combo = com.intellij.openapi.ui.ComboBox(model)

        // Refresh model when combo becomes visible (projects may have loaded since panel created)
        combo.addHierarchyListener { e ->
            if ((e.changeFlags and java.awt.event.HierarchyEvent.SHOWING_CHANGED.toLong()) != 0L && combo.isShowing) {
                val currentProjects = when (nameType) {
                    ProjectNameType.PROJECT_NAME -> allProjects.map { it.projectName }.distinct().sorted()
                    ProjectNameType.CROSS_PROJECT_NAME -> allProjects.map { it.name }.sorted()
                }
                val modelItems = (0 until model.size).map { model.getElementAt(it) }
                if (currentProjects != modelItems) {
                    val selected = combo.selectedItem
                    model.removeAllElements()
                    currentProjects.forEach { model.addElement(it) }
                    if (selected != null && selected in currentProjects) {
                        combo.selectedItem = selected
                    }
                }
            }
        }

        val row = JPanel(FlowLayout(FlowLayout.LEFT, 5, 2)).apply {
            alignmentX = java.awt.Component.LEFT_ALIGNMENT
            add(JBLabel(label))
            add(combo)
            if (required) {
                add(JBLabel("*").apply { foreground = JBColor.RED })
            }
        }
        panel.add(row)

        argsCollector.values[key] = { (combo.selectedItem as? String) ?: "" }
        if (required) {
            argsCollector.required[key] = { combo.selectedItem != null }
        }
    }

    /**
     * Add a multi-select for projects (checkboxes in a scrollable list).
     * Refreshes when panel becomes visible.
     * @param nameType Whether to show base project names or full cross-project names
     */
    private fun addProjectMultiSelect(
        panel: JPanel,
        argsCollector: ArgsCollector,
        label: String,
        required: Boolean,
        nameType: ProjectNameType
    ) {
        val checkboxes = mutableListOf<JCheckBox>()

        val checkboxPanel = JPanel().apply {
            layout = BoxLayout(this, BoxLayout.Y_AXIS)
        }

        val scrollPane = JBScrollPane(checkboxPanel).apply {
            preferredSize = Dimension(300, 100)
            alignmentX = java.awt.Component.LEFT_ALIGNMENT
        }

        // Refresh checkboxes when scrollPane becomes visible
        scrollPane.addHierarchyListener { e ->
            if ((e.changeFlags and java.awt.event.HierarchyEvent.SHOWING_CHANGED.toLong()) != 0L && scrollPane.isShowing) {
                val currentProjects = when (nameType) {
                    ProjectNameType.PROJECT_NAME -> allProjects.map { it.projectName }.distinct().sorted()
                    ProjectNameType.CROSS_PROJECT_NAME -> allProjects.map { it.name }.sorted()
                }
                val existingNames = checkboxes.map { it.text }
                if (currentProjects != existingNames) {
                    val selectedNames = checkboxes.filter { it.isSelected }.map { it.text }.toSet()
                    checkboxes.clear()
                    checkboxPanel.removeAll()
                    currentProjects.forEach { name ->
                        val cb = JCheckBox(name).apply {
                            // Pre-select if previously selected or if the corresponding cross-project is in selection
                            val isInSelection = when (nameType) {
                                ProjectNameType.PROJECT_NAME -> service.getSelectedProjects().any { it.substringBefore('@') == name }
                                ProjectNameType.CROSS_PROJECT_NAME -> name in service.getSelectedProjects()
                            }
                            isSelected = name in selectedNames || isInSelection
                        }
                        checkboxes.add(cb)
                        checkboxPanel.add(cb)
                    }
                    checkboxPanel.revalidate()
                    checkboxPanel.repaint()
                }
            }
        }

        val labelRow = JPanel(FlowLayout(FlowLayout.LEFT, 5, 0)).apply {
            alignmentX = java.awt.Component.LEFT_ALIGNMENT
            add(JBLabel(label))
            if (required) {
                add(JBLabel("*").apply { foreground = JBColor.RED })
            }
        }

        // Select all / deselect all buttons
        val selectAllBtn = JButton("All").apply {
            addActionListener {
                checkboxes.forEach { it.isSelected = true }
            }
        }
        val deselectAllBtn = JButton("None").apply {
            addActionListener {
                checkboxes.forEach { it.isSelected = false }
            }
        }

        val buttonRow = JPanel(FlowLayout(FlowLayout.LEFT, 5, 0)).apply {
            alignmentX = java.awt.Component.LEFT_ALIGNMENT
            add(selectAllBtn)
            add(deselectAllBtn)
        }

        panel.add(labelRow)
        panel.add(buttonRow)
        panel.add(scrollPane)

        argsCollector.multiValues["projects"] = {
            checkboxes.filter { it.isSelected }.map { it.text }
        }

        if (required) {
            argsCollector.required["projects"] = {
                checkboxes.any { it.isSelected }
            }
        }
    }

    /**
     * Add update-deps flags (--steward, --prerelease).
     */
    private fun addUpdateDepsFlags(panel: JPanel, argsCollector: ArgsCollector) {
        val stewardCheckbox = JCheckBox("--steward (use Scala Steward upgrade strategy)")
        val prereleaseCheckbox = JCheckBox("--prerelease (allow prerelease versions)")

        val row = JPanel().apply {
            layout = BoxLayout(this, BoxLayout.Y_AXIS)
            alignmentX = java.awt.Component.LEFT_ALIGNMENT
            add(stewardCheckbox)
            add(prereleaseCheckbox)
        }
        panel.add(row)

        argsCollector.flags["--steward"] = { stewardCheckbox.isSelected }
        argsCollector.flags["--prerelease"] = { prereleaseCheckbox.isSelected }
    }

    /**
     * Run a bleep command in a terminal window.
     */
    private fun runBleepInTerminal(command: String, args: List<String>) {
        ProgressManager.getInstance().run(object : Task.Backgroundable(project, "Preparing bleep command", false) {
            override fun run(indicator: ProgressIndicator) {
                val bleep = service.ensureBleep(indicator)
                if (bleep == null) {
                    ApplicationManager.getApplication().invokeLater {
                        showError("Failed to locate bleep executable")
                    }
                    return
                }

                val basePath = service.projectBasePath ?: return

                ApplicationManager.getApplication().invokeLater {
                    try {
                        // Build command line
                        val fullArgs = mutableListOf(bleep.absolutePath, "-d", basePath.absolutePath)
                        fullArgs.addAll(command.split(" "))
                        fullArgs.addAll(args)

                        val commandLine = GeneralCommandLine(fullArgs)
                            .withWorkDirectory(basePath)
                            .withCharset(Charsets.UTF_8)

                        LOG.info("Running in terminal: ${commandLine.commandLineString}")

                        // Create process handler
                        val processHandler = ProcessHandlerFactory.getInstance()
                            .createColoredProcessHandler(commandLine)

                        // Create run content descriptor and show in Run tool window
                        val console = com.intellij.execution.impl.ConsoleViewImpl(project, true)
                        console.attachToProcess(processHandler)

                        val descriptor = RunContentDescriptor(
                            console,
                            processHandler,
                            console.component,
                            "bleep $command"
                        )

                        val executor = DefaultRunExecutor.getRunExecutorInstance()
                        RunContentManager.getInstance(project).showRunContent(executor, descriptor)

                        processHandler.startNotify()
                    } catch (e: Exception) {
                        LOG.error("Failed to run bleep command", e)
                        showError("Failed to run command: ${e.message}")
                    }
                }
            }
        })
    }
}

/**
 * Cell renderer for the project checkbox tree.
 */
class ProjectTreeCellRenderer : CheckboxTree.CheckboxTreeCellRenderer() {
    override fun customizeRenderer(
        tree: JTree,
        value: Any,
        selected: Boolean,
        expanded: Boolean,
        leaf: Boolean,
        row: Int,
        hasFocus: Boolean
    ) {
        if (value is CheckedTreeNode) {
            val text = value.userObject?.toString() ?: ""
            textRenderer.append(text)

            // Visual indication for test projects
            if (text.contains("test", ignoreCase = true)) {
                textRenderer.append(" (test)", com.intellij.ui.SimpleTextAttributes.GRAYED_ATTRIBUTES)
            }
        }
    }
}
