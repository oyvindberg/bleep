// @ts-check

/** @type {import('@docusaurus/plugin-content-docs').SidebarsConfig} */
const sidebars = {
  tutorialSidebar: [
    {
      type: "doc",
      label: "About",
      id: "readme",
    },
    {
      type: "doc",
      label: "But, my build does much more!",
      id: "my-build-does-more",
    },
    {
      type: "doc",
      label: "File format / model",
      id: "model",
    },
    {
      type: "category",
      label: "Usage",
      collapsible: false,
      items: [
        {
          type: "doc",
          label: "More stable builds",
          id: "usage/stable-builds",
        },
        {
          type: "doc",
          label: "Managing compile servers",
          id: "usage/compile-servers",
        },
        {
          type: "doc",
          label: "Dependencies",
          id: "usage/dependencies",
        },
        {
          type: "doc",
          label: "Selecting projects",
          id: "usage/selecting-projects",
        },
        {
          type: "doc",
          label: "Tab completions",
          id: "usage/tab-completions",
        },
      ],
    },
    {
      type: "doc",
      label: "Porting sbt plugins",
      id: "porting-sbt-plugins",
    },
    {
      type: "doc",
      label: "Installation",
      id: "installing",
    },
    {
      type: "category",
      label: "Comparison to other build tools",
      items: [
        {
          type: "doc",
          label: "sbt",
          id: "compared-to-other-build-tools/sbt",
        },
        {
          type: "doc",
          label: "mill",
          id: "compared-to-other-build-tools/mill",
        },
        {
          type: "doc",
          label: "scala-cli",
          id: "compared-to-other-build-tools/scala-cli",
        },
      ],
    },
    {
      type: "category",
      label: "Demos",
      items: [
        {
          type: "doc",
          label: "create new build (scala native)",
          id: "demos/creating-new-native",
        },
        {
          type: "doc",
          label: "create new cross build",
          id: "demos/creating-new-cross-build",
        },
        {
          type: "doc",
          label: "import sbt build",
          id: "demos/importing-sbt-build",
        },
        {
          type: "doc",
          label: "import build into IDE",
          id: "demos/ide-import",
        },
        {
          type: "doc",
          label: "using CLI",
          id: "demos/cli-experience",
        },
      ],
    },
  ],
};

module.exports = sidebars;
