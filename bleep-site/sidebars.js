// @ts-check

/** @type {import('@docusaurus/plugin-content-docs').SidebarsConfig} */
const sidebars = {
  learn: [
    // GETTING STARTED
    {
      type: "category",
      label: "Getting Started",
      collapsed: false,
      items: [
        {
          type: "doc",
          label: "Installation",
          id: "installing",
        },
        {
          type: "doc",
          label: "Project status",
          id: "appendix/status",
        },
        {
          type: "doc",
          label: "Your First Project (Java)",
          id: "tutorials/your-first-project",
        },
        {
          type: "doc",
          label: "Your First Project (Kotlin)",
          id: "tutorials/your-first-kotlin-project",
        },
        {
          type: "doc",
          label: "Your First Project (Scala)",
          id: "tutorials/your-first-scala-project",
        },
        {
          type: "doc",
          label: "Basic usage",
          id: "tutorials/basic-usage",
        },
        {
          type: "doc",
          label: "IDE setup",
          id: "ide-setup",
        },
        {
          type: "doc",
          label: "MCP server (for AI agents)",
          id: "usage/mcp-server",
        },
        {
          type: "doc",
          label: "Tab completions",
          id: "tab-completions",
        },
      ],
    },

    // CONCEPTS
    {
      type: "category",
      label: "Concepts",
      collapsed: true,
      items: [
        {
          type: "doc",
          label: "Build Model Overview",
          id: "model",
        },
        {
          type: "doc",
          label: "Projects",
          id: "concepts/projects",
        },
        {
          type: "doc",
          label: "Dependencies",
          id: "concepts/dependencies",
        },
        {
          type: "doc",
          label: "Project Layout",
          id: "concepts/project-layout",
        },
        {
          type: "doc",
          label: "Project Globs",
          id: "concepts/project-globs",
        },
        {
          type: "doc",
          label: "Templates & Inheritance",
          id: "concepts/templates-inheritance",
        },
        {
          type: "doc",
          label: "Cross-Building",
          id: "concepts/cross-building",
        },
        {
          type: "category",
          label: "Scripts",
          collapsed: false,
          items: [
            {
              type: "doc",
              label: "Scripts and source generation",
              id: "concepts/scripts",
            },
            {
              type: "doc",
              label: "Bleep scripts",
              id: "concepts/bleep-scripts",
            },
            {
              type: "doc",
              label: "Source generation scripts",
              id: "concepts/sourcegen",
            },
          ],
        },
      ],
    },

    // TUTORIALS
    {
      type: "category",
      label: "Tutorials",
      collapsed: true,
      items: [
        {
          type: "doc",
          label: "Import from Maven",
          id: "demos/importing-maven-build",
        },
        {
          type: "doc",
          label: "Import from sbt",
          id: "demos/importing-sbt-build",
        },
        {
          type: "doc",
          label: "Write Your First Script (Java)",
          id: "tutorials/your-first-script",
        },
        {
          type: "doc",
          label: "Write Your First Script (Kotlin)",
          id: "tutorials/your-first-script-kotlin",
        },
        {
          type: "doc",
          label: "Write Your First Script (Scala)",
          id: "tutorials/your-first-script-scala",
        },
        {
          type: "doc",
          label: "Write Your First Sourcegen (Java)",
          id: "tutorials/your-first-sourcegen",
        },
        {
          type: "doc",
          label: "Write Your First Sourcegen (Kotlin)",
          id: "tutorials/your-first-sourcegen-kotlin",
        },
        {
          type: "doc",
          label: "Write Your First Sourcegen (Scala)",
          id: "tutorials/your-first-sourcegen-scala",
        },
        {
          type: "doc",
          label: "Publish to Maven Central",
          id: "tutorials/publish-to-maven",
        },
      ],
    },

    // GUIDES
    {
      type: "category",
      label: "Guides",
      collapsed: true,
      items: [
        {
          type: "doc",
          label: "Annotation Processing",
          id: "usage/annotation-processing",
        },
        {
          type: "doc",
          label: "CI Project Invalidation",
          id: "usage/ci-invalidation",
        },
        {
          type: "doc",
          label: "CI/CD Setup",
          id: "guides/ci-cd-setup",
        },
        {
          type: "doc",
          label: "Code Formatting",
          id: "usage/fmt",
        },
        {
          type: "doc",
          label: "Compile servers",
          id: "guides/compile-servers",
        },
        {
          type: "doc",
          label: "Conflict resolution",
          id: "usage/conflict-resolution",
        },
        {
          type: "doc",
          label: "Cross-building",
          id: "guides/cross-building",
        },
        {
          type: "doc",
          label: "Inspect the build",
          id: "guides/inspect",
        },
        {
          type: "doc",
          label: "Porting sbt Plugins",
          id: "porting-sbt-plugins",
        },
        {
          type: "category",
          label: "Private Repositories",
          link: {
            type: "doc",
            id: "usage/private-repos/index",
          },
          items: [
            {
              type: "doc",
              label: "Artifactory / generic Maven",
              id: "usage/private-repos/artifactory",
            },
            {
              type: "doc",
              label: "GitHub Packages",
              id: "usage/private-repos/github-packages",
            },
            {
              type: "doc",
              label: "GitLab Package Registry",
              id: "usage/private-repos/gitlab-packages",
            },
            {
              type: "doc",
              label: "Google Artifact Registry",
              id: "usage/private-repos/artifact-registry",
            },
            {
              type: "doc",
              label: "Sonatype / Maven Central",
              id: "usage/private-repos/sonatype",
            },
          ],
        },
        {
          type: "doc",
          label: "Refactor your build",
          id: "guides/refactor",
        },
        {
          type: "doc",
          label: "Remote Build Cache",
          id: "usage/remote-cache",
        },
        {
          type: "doc",
          label: "Resource Filtering",
          id: "guides/resource-filtering",
        },
        {
          type: "doc",
          label: "Unmanaged Jars",
          id: "usage/unmanaged-jars",
        },
      ],
    },

    // COMPARED TO
    {
      type: "category",
      label: "Compared To",
      collapsed: true,
      items: [
        {
          type: "doc",
          label: "Same project, four tools",
          id: "compared-to-other-build-tools/side-by-side",
        },
        {
          type: "doc",
          label: "Maven",
          id: "compared-to-other-build-tools/maven",
        },
        {
          type: "doc",
          label: "Gradle",
          id: "compared-to-other-build-tools/gradle",
        },
        {
          type: "doc",
          label: "sbt",
          id: "compared-to-other-build-tools/sbt",
        },
        {
          type: "doc",
          label: "Mill",
          id: "compared-to-other-build-tools/mill",
        },
        {
          type: "doc",
          label: "scala-cli",
          id: "compared-to-other-build-tools/scala-cli",
        },
      ],
    },

    // APPENDIX
    {
      type: "category",
      label: "Appendix",
      collapsed: true,
      items: [
        {
          type: "doc",
          label: "Glossary",
          id: "appendix/glossary",
        },
        {
          type: "doc",
          label: "Path Replacements",
          id: "appendix/path-replacements",
        },
        {
          type: "doc",
          label: "Source layout: cross-builds & sbt",
          id: "appendix/source-layout-cross-builds",
        },
        {
          type: "doc",
          label: "Stable Builds",
          id: "appendix/stable-builds",
        },
        {
          type: "doc",
          label: "Test Frameworks",
          id: "appendix/test-frameworks",
        },
      ],
    },

  ],

  reference: [
    {
      type: "category",
      label: "CLI commands",
      collapsed: false,
      // Auto-picks every .mdx in docs/reference/cli/. Pages are generated by
      // `bleep gen-cli-docs` from the decline opts tree in bleep-cli/Main.scala.
      items: [{ type: "autogenerated", dirName: "reference/cli" }],
    },
  ],
};

export default sidebars;
