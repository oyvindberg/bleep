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
import java.awt.BorderLayout
import java.awt.FlowLayout
import java.awt.GridBagConstraints
import java.awt.GridBagLayout
import javax.swing.*
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

        // === Tabbed pane ===
        val tabbedPane = JBTabbedPane().apply {
            addTab("Project Picker", projectsTab)
            addTab("Run", scriptsTab)
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

        // Initial load - state machine starts at INITIALIZING
        state = PanelState.INITIALIZING
        refreshProjects()
        checkBspStatus()
    }

    override fun dispose() {
        bspPollTimer.stop()
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
