// @ts-check

/** @type {import('@docusaurus/plugin-content-docs').SidebarsConfig} */
const sidebars = {
  docs: [
    // GETTING STARTED
    {
      type: "category",
      label: "Getting Started",
      collapsed: false,
      items: [
        {
          type: "doc",
          label: "Why Bleep?",
          id: "why-bleep",
        },
        {
          type: "doc",
          label: "Installation",
          id: "installing",
        },
        {
          type: "doc",
          label: "Your First Project",
          id: "tutorials/your-first-project",
        },
        {
          type: "doc",
          label: "IDE Setup",
          id: "demos/ide-import",
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
          label: "Create a Cross-Platform Project",
          id: "demos/creating-new-cross-build",
        },
        {
          type: "doc",
          label: "Import from sbt",
          id: "demos/importing-sbt-build",
        },
        {
          type: "doc",
          label: "Write Your First Script",
          id: "tutorials/your-first-script",
        },
        {
          type: "doc",
          label: "Publish to Maven Central",
          id: "tutorials/publish-to-maven",
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
          label: "Projects & Dependencies",
          id: "concepts/projects-dependencies",
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
          type: "doc",
          label: "Scripts vs Plugins",
          id: "my-build-does-more",
        },
        {
          type: "doc",
          label: "Build Rewrites",
          id: "usage/build-rewrites",
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
          label: "Dependency Management",
          id: "usage/dependencies",
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
          label: "Porting sbt Plugins",
          id: "porting-sbt-plugins",
        },
        {
          type: "doc",
          label: "Annotation Processing",
          id: "usage/annotation-processing",
        },
      ],
    },

    // REFERENCE
    {
      type: "category",
      label: "Reference",
      collapsed: true,
      items: [
        {
          type: "category",
          label: "CLI Commands",
          collapsed: true,
          items: [
            { type: "doc", label: "Overview", id: "reference/cli/overview" },
            { type: "doc", label: "compile", id: "reference/cli/compile" },
            { type: "doc", label: "test", id: "reference/cli/test" },
            { type: "doc", label: "run", id: "reference/cli/run" },
            { type: "doc", label: "build", id: "reference/cli/build" },
            { type: "doc", label: "fmt", id: "reference/cli/fmt" },
            { type: "doc", label: "setup-ide", id: "reference/cli/setup-ide" },
            { type: "doc", label: "import", id: "reference/cli/import" },
            { type: "doc", label: "publish-local", id: "reference/cli/publish-local" },
          ],
        },
        {
          type: "category",
          label: "bleep.yaml Schema",
          collapsed: true,
          items: [
            { type: "doc", label: "Overview", id: "reference/schema/overview" },
            { type: "doc", label: "Projects", id: "reference/schema/projects" },
            { type: "doc", label: "Templates", id: "reference/schema/templates" },
            { type: "doc", label: "Scripts", id: "reference/schema/scripts" },
            { type: "doc", label: "Dependencies", id: "reference/schema/dependencies" },
            { type: "doc", label: "Platform Configuration", id: "reference/schema/platform" },
          ],
        },
        {
          type: "category",
          label: "Build Environment",
          collapsed: true,
          items: [
            { type: "doc", label: "Stable Builds", id: "usage/stable-builds" },
            { type: "doc", label: "Compile Servers", id: "usage/compile-servers" },
            { type: "doc", label: "Project Selection", id: "usage/selecting-projects" },
            { type: "doc", label: "Tab Completions", id: "usage/tab-completions" },
          ],
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
          label: "FAQ",
          id: "appendix/faq",
        },
        {
          type: "doc",
          label: "Troubleshooting",
          id: "appendix/troubleshooting",
        },
        {
          type: "doc",
          label: "Glossary",
          id: "appendix/glossary",
        },
      ],
    },

    // DEMOS
    {
      type: "category",
      label: "Demos",
      collapsed: true,
      items: [
        {
          type: "doc",
          label: "Create Scala Native Build",
          id: "demos/creating-new-native",
        },
        {
          type: "doc",
          label: "CLI Experience",
          id: "demos/cli-experience",
        },
      ],
    },
  ],
};

module.exports = sidebars;
