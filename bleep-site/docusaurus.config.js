// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

import { themes as prismThemes } from "prism-react-renderer";

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: "Bleep",
  tagline: "A Bleeping Fast Build Tool",
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
    [
      "@docusaurus/plugin-client-redirects",
      {
        redirects: [
          { from: "/docs", to: "/docs/installing/" },
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
