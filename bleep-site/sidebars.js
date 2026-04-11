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
                    label: "Scripts",
                    id: "usage/scripts",
                },
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
                    label: "Java Annotation Processing",
                    id: "usage/annotation-processing",
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
                }, {
                    type: "doc",
                    label: "Build rewrites",
                    id: "usage/build-rewrites",
                }, {
                    type: "doc",
                    label: "Source generation",
                    id: "usage/sourcegen",
                }, {
                    type: "doc",
                    label: "CI project invalidation",
                    id: "usage/ci-invalidation",
                }, {
                    type: "doc",
                    label: "Remote build cache",
                    id: "usage/remote-cache",
                }, {
                    type: "category",
                    label: "Private repositories",
                    link: {
                        type: "doc",
                        id: "usage/private-repos/index",
                    },
                    items: [
                        {
                            type: "doc",
                            label: "Google Artifact Registry",
                            id: "usage/private-repos/artifact-registry",
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
                            label: "Sonatype / Maven Central",
                            id: "usage/private-repos/sonatype",
                        },
                    ],
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
