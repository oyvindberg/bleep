// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

import { themes as prismThemes } from "prism-react-renderer";
import { latestBleepVersion } from "./scripts/latest-version.mjs";
import remarkBleepVersion from "./scripts/remark-bleep-version.mjs";

// Resolved once per build from git tags. Docs write `BLEEP_LATEST_VERSION` and get this substituted in, so a release is
// published by tagging it and nothing in docs/ has to be edited. Exposed as a custom field too, for the React pages
// under src/pages/ which are not MDX and so never reach the remark plugin.
const bleepVersion = latestBleepVersion();

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: "Bleep",
  tagline: "A Bleeping Fast Build Tool",
  customFields: { bleepVersion },
  url: "https://bleep.build",
  baseUrl: "/",
  onBrokenLinks: "throw",
  favicon: "img/bleep-logo-mark.svg",
  markdown: {
    hooks: {
      onBrokenMarkdownLinks: "warn",
    },
  },

  // GitHub pages deployment config.
  organizationName: "oyvindberg",
  projectName: "bleep",
  trailingSlash: true,

  i18n: {
    defaultLocale: "en",
    locales: ["en"],
  },

  plugins: [
    "./scripts/snippet-extractor-plugin.js",
    [
      "@docusaurus/plugin-client-redirects",
      {
        redirects: [
          { from: "/docs", to: "/docs/installing/" },
          // `docs/usage/private-repositories.mdx` duplicated the whole `usage/private-repos/`
          // directory and was in no sidebar. Merged into the index there; keep the old URL alive.
          { from: "/docs/usage/private-repositories", to: "/docs/usage/private-repos/" },
        ],
      },
    ],
  ],

  presets: [
    [
      "classic",
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          path: "../docs",
          sidebarPath: "./sidebars.js",
          remarkPlugins: [[remarkBleepVersion, { version: bleepVersion }]],
        },
        theme: {
          customCss: "./src/css/custom.css",
        },
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      navbar: {
        title: "Bleep",
        logo: {
          alt: "Bleep Logo",
          src: "img/bleep-logo-mark.svg",
        },
        items: [
          {
            type: "docSidebar",
            sidebarId: "learn",
            position: "left",
            label: "Learn",
          },
          {
            type: "docSidebar",
            sidebarId: "reference",
            position: "left",
            label: "Reference",
          },
          {
            href: "https://github.com/oyvindberg/bleep",
            label: "GitHub",
            position: "right",
          },
        ],
      },
      footer: {
        style: "dark",
        links: [
          {
            title: "Learn",
            items: [
              {
                label: "Installation",
                to: "/docs/installing",
              },
              {
                label: "Tutorials",
                to: "/docs/tutorials/your-first-project",
              },
            ],
          },
          {
            title: "Community",
            items: [
              {
                label: "GitHub",
                href: "https://github.com/oyvindberg/bleep",
              },
              {
                label: "Discussions",
                href: "https://github.com/oyvindberg/bleep/discussions",
              },
              {
                label: "Releases",
                href: "https://github.com/oyvindberg/bleep/releases",
              },
            ],
          },
        ],
        copyright: `Copyright ${new Date().getFullYear()} Bleep Contributors. Built with Docusaurus.`,
      },
      prism: {
        theme: prismThemes.github,
        darkTheme: prismThemes.oceanicNext,
        additionalLanguages: ["java", "scala", "yaml", "bash"],
      },
    }),
};

export default config;
